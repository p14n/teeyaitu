(ns com.p14n.teeyaitu.features.interface
  (:require [com.p14n.teeyaitu.features.core :as core]))

(def raw-feature-cols
  "All computed features (before ranking they hold raw values)."
  core/raw-feature-cols)

(def model-feature-cols
  "Ranked columns the model trains on. Never contains a target column."
  (core/assert-no-leaky-features! core/model-feature-cols))

(def target-cols core/target-cols)

(defn build
  "Monthly panel (from panel/monthly-panel) -> dataset with ranked features and targets.
  Rows lacking 13 months of history are dropped. opts: :member? (fn [ticker date] bool)."
  ([monthly] (core/build monthly))
  ([monthly opts] (core/build monthly opts)))

(defn training-rows [ds target] (core/training-rows ds target))

(defn assert-no-leaky-features! [cols] (core/assert-no-leaky-features! cols))

;; Steps, exposed for tests and research.
(defn add-time-series [monthly] (core/add-time-series monthly))
(defn complete-history? [row] (core/complete-history? row))
(defn rank-features [rows] (core/rank-features rows))
(defn add-targets [rows] (core/add-targets rows))
(defn percentile-ranks [xs] (core/percentile-ranks xs))
