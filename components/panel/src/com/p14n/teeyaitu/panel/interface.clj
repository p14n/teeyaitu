(ns com.p14n.teeyaitu.panel.interface
  (:require [com.p14n.teeyaitu.panel.core :as core]))

(defn monthly-panel
  "Daily raw prices (dataset or row maps) -> dataset with one row per ticker per month:
  :ticker :month (YearMonth) :date (last trading day) :close :adj-close :vol-60d :high-252d.
  Not filtered by index membership; do that after computing features.
  opts: :vol-window (60), :high-window (252)."
  ([daily] (core/monthly-panel daily))
  ([daily opts] (core/monthly-panel daily opts)))

(defn add-daily-stats [daily opts] (core/add-daily-stats daily opts))

(defn month-end [daily] (core/month-end daily))

(defn rolling-vol [prices window] (core/rolling-vol prices window))
