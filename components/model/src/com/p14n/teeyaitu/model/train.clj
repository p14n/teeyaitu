(ns com.p14n.teeyaitu.model.train
  "XGBoost regression over the monthly feature panel, trained per walk-forward
  split via metamorph.ml. Option keys follow scicloj.ml.xgboost: boosting rounds
  are :round (there is no :iterations option), plus :eta, :max-depth, :lambda,
  :alpha, :subsample, :gamma, :min-child-weight, :early-stopping-round."
  (:require [scicloj.metamorph.core :as mm]
            [scicloj.metamorph.ml :as ml]
            [scicloj.ml.xgboost] ;; require for side effect: registers :xgboost/* model types
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.metamorph :as ds-mm]))

(def default-xgb-opts
  "Defaults for a monthly momentum panel. :model-type :xgboost/regression is
  XGBoost reg:squarederror; :round is the number of boosting rounds."
  {:model-type :xgboost/regression
   :round 50
   :eta 0.05
   :max-depth 3
   :subsample 0.8
   :lambda 1.0})

(defn make-pipeline
  "metamorph pipeline that sets the inference target and trains/predicts an
  XGBoost regression depending on :metamorph/mode."
  [target xgb-opts]
  (mm/pipeline
   (ds-mm/set-inference-target target)
   (ml/model (merge default-xgb-opts xgb-opts))))

(defn- rows
  "Accepts a dataset or a seq of row maps."
  [panel]
  (if (ds/dataset? panel) (tc/rows panel :as-maps) (vec panel)))

(defn- check-opts!
  [{:keys [feature-cols target]}]
  (when-not (and (seq feature-cols) (keyword? target))
    (throw (ex-info "train: opts need :feature-cols (seq of columns) and :target (keyword)"
                    {:feature-cols feature-cols :target target}))))

(defn- complete?
  "True when every feature column holds a value for the row."
  [fcols r]
  (every? #(some? (get r %)) fcols))

(defn train
  "panel + one walk-forward split {:train-months [...]} + opts {:keys
  :feature-cols :target :xgb-opts} -> {:keys :pipe-fn :ctx}, a fitted
  metamorph pipeline. Trains on the split's train-month rows that have every
  feature and a non-nil target."
  [panel {:keys [train-months]} {:keys [feature-cols target xgb-opts] :as opts}]
  (check-opts! opts)
  (let [train-set    (set train-months)
        fcols        (vec feature-cols)
        train-rows   (->> (rows panel)
                          (filter #(and (train-set (:month %))
                                        (some? (get % target))
                                        (complete? fcols %)))
                          (mapv #(assoc (zipmap fcols (map % fcols))
                                       target
                                       (double (get % target)))))]
    (when (empty? train-rows)
      (throw (ex-info "train: no complete rows in the split's train months"
                      {:train-months train-months :target target})))
    (let [pipe (make-pipeline target xgb-opts)]
      {:pipe-fn pipe
       :ctx    (pipe {:metamorph/data (ds/->dataset train-rows)
                     :metamorph/mode :fit})})))

(defn predict
  "panel + one walk-forward split {:test-months [...]} + fitted (from train) +
  opts {:keys :feature-cols :target} -> dataset of {:month :ticker
  :prediction <target>} for every test-month row with a complete feature set.
  The target column carries the realised forward return (nil at the panel
  edge), kept for downstream ranking and metrics."
  [panel {:keys [test-months]} {:keys [pipe-fn ctx]} {:keys [feature-cols target] :as opts}]
  (check-opts! opts)
  (let [test-set  (set test-months)
        fcols     (vec feature-cols)
        test-rows (vec (filter #(and (test-set (:month %))
                                     (complete? fcols %))
                               (rows panel)))
        empty-ds  (ds/->dataset {:month [] :ticker [] :prediction [] target []})]
    (if (empty? test-rows)
      empty-ds
      ;; The pipeline re-runs set-inference-target in :transform mode, so the
      ;; prediction data must carry the target column; metamorph.ml predicts on
      ;; the non-target (feature) columns only, so its values never matter.
      (let [feature-ds (ds/->dataset
                        (mapv (fn [r]
                                (assoc (zipmap fcols (map r fcols))
                                       target
                                       (double (or (get r target) 0.0))))
                              test-rows))
            pred-ds    (-> (mm/transform-pipe feature-ds pipe-fn ctx) :metamorph/data)
            preds      (vec (get pred-ds target))]
        (ds/->dataset
         (mapv (fn [r p]
                 {:month (:month r)
                  :ticker (:ticker r)
                  :prediction (double p)
                  target (some-> (get r target) double)})
               test-rows preds))))))

(defn run-walk-forward
  "panel + splits (from walk-forward) + opts {:keys :feature-cols :target
  :xgb-opts} -> one dataset of out-of-sample predictions over every split's
  test window, ordered by :month then :ticker. Trains a fresh pipeline per
  split; an empty splits sequence yields an empty dataset."
  [panel splits {:keys [feature-cols target xgb-opts] :as opts}]
  (check-opts! opts)
  (let [rows (mapcat #(tc/rows % :as-maps)
                     (mapv (fn [split]
                             (let [fitted (train panel split opts)]
                               (predict panel split fitted opts)))
                           splits))]
    (if (empty? rows)
      (ds/->dataset {:month [] :ticker [] :prediction [] (:target opts) []})
      (tc/order-by (ds/->dataset rows) [:month :ticker]))))
