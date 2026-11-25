(ns dev.research
  "Phase 1 research loop at the REPL: Yahoo backfill -> Parquet -> monthly panel ->
  features -> baseline and equal-weight backtests. Start a REPL with `clj -M:dev:test`."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [tablecloth.api :as tc]
            [com.p14n.teeyaitu.backtest.interface :as backtest]
            [com.p14n.teeyaitu.config.interface :as config]
            [com.p14n.teeyaitu.costs.interface :as costs]
            [com.p14n.teeyaitu.features.interface :as features]
            [com.p14n.teeyaitu.panel.interface :as panel]
            [com.p14n.teeyaitu.price-store.interface :as store]
            [com.p14n.teeyaitu.universe.interface :as universe]
            [com.p14n.teeyaitu.yahoo-client.interface :as yahoo])
  (:import (java.time LocalDate)))

(defn git-sha []
  (let [{:keys [exit out]} (sh/sh "git" "rev-parse" "HEAD")]
    (when (zero? exit) (str/trim out))))

(defn backfill!
  "Fetches Yahoo history for every ticker, checks units, writes Parquet. Returns
  {:ok [...] :failed {ticker reason}}; a ticker that fails the pence check is not written."
  [cfg membership {:keys [from to] :or {from (LocalDate/of 2000 1 1)}}]
  (let [root (get-in cfg [:data :root])]
    (reduce
     (fn [acc ticker]
       (try
         (let [ds (yahoo/fetch-daily ticker (merge (:yahoo cfg) {:from from :to to}))]
           (store/assert-quality! ds)
           (store/write-prices! root ds)
           (println "ok" ticker (tc/row-count ds))
           (update acc :ok conj ticker))
         (catch Exception e
           (println "FAILED" ticker (ex-message e))
           (assoc-in acc [:failed ticker] (merge {:message (ex-message e)} (ex-data e))))))
     {:ok [] :failed {}}
     (universe/tickers membership))))

(defn load-sonia
  "Reads a Bank of England SONIA CSV export (DATE,IUDSOIA) saved under the data root."
  [cfg]
  (let [f (io/file (get-in cfg [:data :root]) "sonia" "IUDSOIA.csv")]
    (when-not (.exists f)
      (throw (ex-info "Download SONIA (series IUDSOIA) from the Bank of England database as CSV"
                      {:expected (.getPath f)})))
    (costs/sonia-lookup (costs/sonia-series (costs/parse-boe-csv (slurp f))))))

(defn feature-dataset [cfg membership]
  (let [daily (store/read-prices (get-in cfg [:data :root]) {:sources #{:yahoo}})]
    (features/build (panel/monthly-panel daily (:features cfg))
                    {:member? #(universe/member? membership %1 %2)})))

(defn baseline-report
  "Baseline 12-1 momentum vs equal weight, net of costs, with a reproducibility record."
  [cfg membership feature-ds sonia]
  (let [report (backtest/compare-strategies
                feature-ds
                (merge (:portfolio cfg)
                       {:costs    (:costs cfg)
                        :sonia    sonia
                        :warnings [(universe/survivorship-warning membership)]}))]
    (assoc report :run {:at (str (java.time.Instant/now))
                        :git-sha (git-sha)
                        :survivorship-biased? (universe/survivorship-biased? membership)
                        :membership (select-keys membership [:index :as-of :source])})))

(defn save-run!
  "Writes a report (without the monthly series) under <root>/runs/ for reproducibility."
  [cfg report]
  (let [f (io/file (get-in cfg [:data :root]) "runs" (str (str/replace (get-in report [:run :at]) ":" "-") ".edn"))]
    (io/make-parents f)
    (spit f (with-out-str
              (pp/pprint (-> report
                             (update :baseline dissoc :monthly)
                             (update :equal-weight dissoc :monthly)))))
    (.getPath f)))

(comment
  (def cfg (config/load-config))
  (def membership (universe/load-membership))

  ;; 1. One-off backfill (about 100 requests at 1/s; responses cached under .cache/yahoo)
  (def result (backfill! cfg membership {:from (LocalDate/of 2000 1 1)}))
  (:failed result)

  ;; 2. Panel and features
  (def ds (feature-dataset cfg membership))
  (tc/info ds)

  ;; 3. Baseline backtest (needs data/sonia/IUDSOIA.csv)
  (def sonia (load-sonia cfg))
  (def report (baseline-report cfg membership ds sonia))
  (:warnings report)
  (get-in report [:baseline :overall])
  (get-in report [:equal-weight :overall])
  (save-run! cfg report))
