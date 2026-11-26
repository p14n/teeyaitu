(ns com.p14n.teeyaitu.model.interface
  (:require [com.p14n.teeyaitu.model.core :as core]))

(def default-opts
  "Five-year train window, two-quarter test window rolling by one test window,
  three-month purge for the 1m/3m forward-return labels."
  core/default-opts)

(defn walk-forward
  "Sorted panel months + opts {:train-months :test-months :step-months
  :purge-months :label-horizon} -> vector of {:train-months [...]
  :test-months [...]}. The purge gap (default: the label horizon) guarantees no
  training month's forward-return label window overlaps a test month. Too few
  months for one full split returns an empty vector."
  ([months] (core/walk-forward months))
  ([months opts] (core/walk-forward months opts)))
