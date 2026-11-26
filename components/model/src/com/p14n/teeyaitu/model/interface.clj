(ns com.p14n.teeyaitu.model.interface
  (:require [com.p14n.teeyaitu.model.core :as core]
            [com.p14n.teeyaitu.model.train :as train]))

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

(def default-xgb-opts
  "XGBoost regression defaults for the monthly momentum panel. Option keys are
  scicloj.ml.xgboost's: :round is the number of boosting rounds (there is no
  :iterations option)."
  train/default-xgb-opts)

(defn train
  "panel (features dataset, or seq of row maps) + one walk-forward split + opts
  {:keys :feature-cols :target :xgb-opts} -> {:keys :pipe-fn :ctx}, a fitted
  metamorph.ml XGBoost pipeline. Pass features/model-feature-cols as
  :feature-cols and :fwd-ret-1m / :fwd-ret-3m as :target. Only rows in the
  split's train months with every feature and a non-nil target are used."
  [panel split opts] (train/train panel split opts))

(defn predict
  "panel + one walk-forward split + fitted (from train) + opts {:keys
  :feature-cols :target} -> dataset of {:month :ticker :prediction <target>}
  for the split's test months, realised target kept for ranking and metrics."
  [panel split fitted opts] (train/predict panel split fitted opts))

(defn run-walk-forward
  "panel + splits (from walk-forward) + opts {:keys :feature-cols :target
  :xgb-opts} -> concatenated out-of-sample predictions over every split's test
  window, ordered by :month then :ticker. Trains a fresh pipeline per split."
  [panel splits opts] (train/run-walk-forward panel splits opts))
