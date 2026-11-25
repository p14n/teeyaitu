(ns com.p14n.teeyaitu.backtest.core
  "Monthly-rebalanced portfolio simulation net of spread-bet costs.

  Input rows (dataset or maps) need :month (YearMonth) :date (month-end LocalDate)
  :ticker, a score column, a one-month forward return column (default :fwd-ret-1m)
  and a raw volatility column (default :vol-60d-raw). Positions are set at each month-end
  close from that month's scores only and held for one month; weights drift with returns,
  and turnover and costs come from the actual weight changes."
  (:require [tablecloth.api :as tc]
            [com.p14n.teeyaitu.costs.interface :as costs])
  (:import (java.time LocalDate YearMonth)))

(def default-opts
  {:score-col  :score
   :return-col :fwd-ret-1m
   :vol-col    :vol-60d-raw
   :select     :top          ; :top quantile by score, or :all
   :quantile   0.2
   :weighting  :inverse-vol  ; or :equal
   :target-vol 0.15          ; nil => weights sum to 1 before caps
   :max-weight 0.10          ; nil => no per-stock cap
   :max-gross  1.0           ; nil => no leverage cap
   :horizon    1             ; >1 => that many staggered sleeves
   :costs      costs/defaults
   :sonia      nil})         ; fn of LocalDate -> annual rate; required

(defn ->rows [data]
  (if (tc/dataset? data) (tc/rows data :as-maps) data))

;; --- target weights ------------------------------------------------------------------

(defn- select-rows [rows {:keys [select score-col quantile]}]
  (if (= :all select)
    rows
    (let [scored (filter #(some? (get % score-col)) rows)
          n      (max 1 (Math/round (* (double quantile) (count scored))))]
      (take n (sort-by #(- (double (get % score-col))) scored)))))

(defn target-weights
  "ticker -> weight (fraction of capital) for one month's rows."
  [rows {:keys [weighting vol-col target-vol max-weight max-gross] :as opts}]
  (let [chosen (cond->> (select-rows rows opts)
                 (= :inverse-vol weighting) (filter #(let [v (get % vol-col)] (and v (pos? (double v))))))
        raw    (map (fn [r] [r (if (= :inverse-vol weighting) (/ 1.0 (double (get r vol-col))) 1.0)]) chosen)
        total  (reduce + (map second raw))]
    (if (or (empty? raw) (not (pos? total)))
      {}
      (let [w (map (fn [[r x]] [r (/ x total)]) raw)
            ;; Ex-ante portfolio vol assuming perfect correlation (sum of w*sigma): a
            ;; conservative estimate, so realised vol will usually be below target.
            w (if target-vol
                (let [port-vol (reduce + (map (fn [[r x]] (* x (double (or (get r vol-col) 0.0)))) w))]
                  (if (pos? port-vol)
                    (map (fn [[r x]] [r (* x (/ (double target-vol) port-vol))]) w)
                    w))
                w)
            w (if max-weight (map (fn [[r x]] [r (min x (double max-weight))]) w) w)
            g (reduce + (map second w))
            w (if (and max-gross (> g (double max-gross)))
                (map (fn [[r x]] [r (* x (/ (double max-gross) g))]) w)
                w)]
        (into {} (map (fn [[r x]] [(:ticker r) x])) w)))))

;; --- simulation ------------------------------------------------------------------------

(defn- abs-diff-sum [a b]
  (reduce + (map (fn [k] (Math/abs (- (double (get a k 0.0)) (double (get b k 0.0)))))
                 (distinct (concat (keys a) (keys b))))))

(defn- step-sleeve
  "One month for one sleeve. Weights are fractions of the sleeve's NAV."
  [{:keys [nav w]} rebalance-to rets nights sonia {:keys [costs]}]
  (let [w-before w
        w-after  (or rebalance-to w-before)
        traded   (abs-diff-sum w-after w-before)
        tcost    (costs/trade-cost traded costs)
        gross    (reduce + (map (fn [[t x]] (* x (get rets t 0.0))) w-after))
        funding  (reduce + (map (fn [[_ x]] (costs/funding-cost x nights sonia costs)) w-after))
        net      (- gross tcost funding)
        growth   (+ 1.0 net)
        drifted  (if (pos? growth)
                   (into {} (map (fn [[t x]] [t (/ (* x (+ 1.0 (get rets t 0.0))) growth)])) w-after)
                   {})]
    {:sleeve {:nav (* nav growth) :w drifted}
     :nav0 nav :gross gross :net net :trade-cost tcost :funding funding :traded traded
     :gross-exposure (reduce + (map (comp #(Math/abs (double %)) val) w-after))
     :positions (count w-after)}))

(defn run
  "Simulates the strategy; returns a vector of monthly results with :month :date :gross
  :net :trade-cost :funding :traded :turnover :gross-exposure :positions :missing-returns."
  [data opts]
  (let [{:keys [return-col horizon sonia] :as opts} (merge default-opts opts)
        _       (when-not sonia (throw (ex-info "Backtest needs :sonia (fn of date -> rate)" {})))
        by-m    (group-by :month (->rows data))
        months  (->> (keys by-m)
                     (filter (fn [m] (some #(some? (get % return-col)) (by-m m))))
                     sort vec)
        h       (long horizon)]
    (loop [i 0
           sleeves (vec (repeat h {:nav (/ 1.0 h) :w {}}))
           out []]
      (if (>= i (count months))
        out
        (let [^YearMonth m (months i)
              rows    (by-m m)
              ^LocalDate date (reduce (fn [^LocalDate a ^LocalDate b] (if (.isAfter b a) b a))
                                      (map :date rows))
              nights  (costs/nights date (.atEndOfMonth (.plusMonths m 1)))
              rets    (into {} (map (fn [r] [(:ticker r) (double (or (get r return-col) 0.0))])) rows)
              missing (count (filter #(nil? (get % return-col)) rows))
              s-rate  (sonia date)
              target  (delay (target-weights rows opts))
              steps   (vec (map-indexed
                            (fn [k sl]
                              (let [rebalance? (and (>= i k) (zero? (mod (- i k) h)))]
                                (step-sleeve sl (when rebalance? @target) rets nights s-rate opts)))
                            sleeves))
              nav0    (reduce + (map :nav0 steps))
              agg     (fn [kw] (/ (reduce + (map #(* (:nav0 %) (kw %)) steps)) nav0))]
          (recur (inc i)
                 (mapv :sleeve steps)
                 (conj out {:month m :date date
                            :gross (agg :gross) :net (agg :net)
                            :trade-cost (agg :trade-cost) :funding (agg :funding)
                            :traded (agg :traded) :turnover (/ (agg :traded) 2.0)
                            :gross-exposure (agg :gross-exposure)
                            :positions (reduce + (map :positions steps))
                            :missing-returns missing})))))))

;; --- metrics ---------------------------------------------------------------------------

(defn- mean [xs] (when (seq xs) (/ (reduce + xs) (count xs))))

(defn- stdev [xs]
  (let [n (count xs)]
    (when (> n 1)
      (let [m (mean xs)]
        (Math/sqrt (/ (reduce + (map #(Math/pow (- % m) 2) xs)) (dec n)))))))

(defn max-drawdown
  "Largest peak-to-trough fall of the compounded NAV, as a positive fraction."
  [rets]
  (loop [[r & more] rets nav 1.0 peak 1.0 dd 0.0]
    (if (nil? r)
      dd
      (let [nav (* nav (+ 1.0 r)) peak (max peak nav)]
        (recur more nav peak (max dd (- 1.0 (/ nav peak))))))))

(defn metrics
  "Annual return (geometric), volatility, Sharpe (on net returns, no risk-free deduction:
  funding already charges SONIA), max drawdown, turnover and cost drag."
  [results]
  (let [net   (map :net results)
        gross (map :gross results)
        n     (count results)]
    (when (pos? n)
      (let [growth (reduce * (map #(+ 1.0 %) net))
            vol    (some-> (stdev net) (* (Math/sqrt 12.0)))]
        {:months           n
         :annual-return    (if (pos? growth) (- (Math/pow growth (/ 12.0 n)) 1.0) -1.0)
         :annual-vol       vol
         :sharpe           (when (and vol (pos? vol)) (/ (* 12.0 (mean net)) vol))
         :max-drawdown     (max-drawdown net)
         :monthly-turnover (mean (map :turnover results))
         :cost-drag        (* 12.0 (- (mean gross) (mean net)))
         :mean-monthly-net (mean net)}))))

(defn metrics-by-year [results]
  (into (sorted-map)
        (map (fn [[y rs]] [y (metrics rs)]))
        (group-by #(.getYear ^YearMonth (:month %)) results)))

(defn beat-share
  "Share of common months where `a` had the higher net return than `b`."
  [a b]
  (let [bm     (into {} (map (juxt :month :net)) b)
        common (filter #(contains? bm (:month %)) a)]
    (when (seq common)
      (/ (double (count (filter #(> (:net %) (bm (:month %))) common))) (count common)))))

;; --- strategy comparison and gate -------------------------------------------------------

(defn summarise [results]
  {:monthly results :overall (metrics results) :by-year (metrics-by-year results)})

(defn compare-strategies
  "Runs the model, the 12-1 momentum baseline and an equal-weight portfolio of all rows on
  identical months. opts as for `run`, plus :baseline-col (default :mom-12-1) and
  :warnings (e.g. the universe survivorship warning)."
  [data {:keys [score-col baseline-col warnings] :or {baseline-col :mom-12-1} :as opts}]
  (let [rows     (->rows data)
        model    (when score-col (run rows (assoc opts :score-col score-col)))
        baseline (run rows (assoc opts :score-col baseline-col))
        ew       (run rows (assoc opts :select :all :weighting :equal :target-vol nil
                                  :max-weight nil :max-gross nil :horizon 1))]
    (cond-> {:config      (dissoc (merge default-opts opts) :sonia :warnings)
             :warnings    (vec (remove nil? warnings))
             :baseline    (summarise baseline)
             :equal-weight (summarise ew)}
      model (assoc :model (summarise model)
                   :model-beat-baseline-share (beat-share model baseline)))))

(defn promote?
  "Gate: model net Sharpe beats the baseline's and its max drawdown is no more than
  `max-dd-multiple` (1.25) times the baseline's."
  ([model-metrics baseline-metrics] (promote? model-metrics baseline-metrics 1.25))
  ([model-metrics baseline-metrics max-dd-multiple]
   (let [ms (:sharpe model-metrics) bs (:sharpe baseline-metrics)]
     (boolean
      (and ms bs (> ms bs)
           (<= (:max-drawdown model-metrics)
               (* max-dd-multiple (:max-drawdown baseline-metrics))))))))
