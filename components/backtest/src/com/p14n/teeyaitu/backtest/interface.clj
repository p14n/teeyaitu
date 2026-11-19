(ns com.p14n.teeyaitu.backtest.interface
  (:require [com.p14n.teeyaitu.backtest.core :as core]))

(def default-opts
  "Long-only top quintile, inverse-vol weights scaled to 15% target vol, 10% cap,
  monthly rebalance, IG cost defaults. :sonia must be supplied."
  core/default-opts)

(defn run
  "Simulates one strategy; returns monthly results (gross, net, costs, turnover)."
  [data opts]
  (core/run data opts))

(defn target-weights [rows opts] (core/target-weights rows (merge core/default-opts opts)))

(defn metrics [results] (core/metrics results))

(defn metrics-by-year [results] (core/metrics-by-year results))

(defn max-drawdown [rets] (core/max-drawdown rets))

(defn beat-share [a b] (core/beat-share a b))

(defn compare-strategies
  "Model (:score-col), 12-1 baseline and equal-weight on identical months, with
  :warnings carried into the report."
  [data opts]
  (core/compare-strategies data opts))

(defn promote?
  "Promotion gate: better net Sharpe and max drawdown <= 1.25x baseline's."
  ([model-metrics baseline-metrics] (core/promote? model-metrics baseline-metrics))
  ([model-metrics baseline-metrics max-dd-multiple]
   (core/promote? model-metrics baseline-metrics max-dd-multiple)))
