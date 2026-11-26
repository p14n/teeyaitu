(ns com.p14n.teeyaitu.model.core
  "Walk-forward splits over the monthly feature panel with a purge gap wide
  enough that no training month's forward-return label window overlaps a test
  month. Pure split logic; no ML dependencies."
  (:import (java.time YearMonth)
           (java.time.temporal ChronoUnit)))

(def default-opts
  "Panel defaults: five years of training, two quarters of testing, rolling by
  one test window, three-month purge for the 1m/3m forward-return labels."
  {:train-months 60 :test-months 6 :label-horizon 3})

(defn- check!
  "Throws ex-info with msg when ok is falsy, naming the offending key."
  [ok k msg opts]
  (when-not ok
    (throw (ex-info msg (assoc opts :bad-key k)))))

(defn- validate-opts!
  [opts]
  (doseq [k [:train-months :test-months :step-months :label-horizon]]
    (check! (pos? (get opts k)) k
            (str "walk-forward: " (name k) " must be positive, got " (get opts k)) opts))
  (check! (>= (:purge-months opts) (:label-horizon opts)) :purge-months
          (str "walk-forward: :purge-months " (:purge-months opts)
               " is narrower than :label-horizon " (:label-horizon opts)
               ", so forward-return labels would leak from train into test")
          opts)
  opts)

(defn- validate-months!
  [ms]
  (check! (every? #(instance? YearMonth %) ms) :months
          "walk-forward: panel months must be java.time.YearMonth" {})
  (check! (or (empty? ms) (apply distinct? ms)) :months
          "walk-forward: duplicate panel months" {})
  ms)

(defn month-dist
  "Months from a to b (positive when b is later)."
  [^YearMonth a ^YearMonth b]
  (.until a b ChronoUnit/MONTHS))

(defn- split-at-index
  "One split anchored at train-start index i, or nil when the panel is too
  short. The test window starts at the first month more than :purge-months
  after the train end, which keeps every train label window clear of every
  test month even if the panel has gaps."
  [ms {:keys [train-months test-months purge-months]} i]
  (let [train-end-idx (+ i train-months -1)]
    (when (< train-end-idx (count ms))
      (let [train     (subvec ms i (inc train-end-idx))
            train-end (nth ms train-end-idx)
            test-idx  (some (fn [idx]
                              (when (> (month-dist train-end (nth ms idx)) purge-months) idx))
                            (range (inc train-end-idx) (count ms)))]
        (when (and test-idx (<= (+ test-idx test-months) (count ms)))
          {:train-months train
           :test-months  (subvec ms test-idx (+ test-idx test-months))})))))

(defn walk-forward
  "Sorted panel months + opts -> vector of {:train-months [...] :test-months [...]}.
  The train window is fixed-size and rolls forward by :step-months (defaults to
  :test-months); each test window follows the train window after a gap of
  :purge-months (defaults to :label-horizon and must be at least as wide, so no
  training month's label window overlaps a test month). Fewer months than one
  full split yields an empty vector."
  ([months-seq] (walk-forward months-seq default-opts))
  ([months-seq opts]
   (let [opts    (merge default-opts opts)
         opts    (if (contains? opts :purge-months)
                   opts
                   (assoc opts :purge-months (:label-horizon opts)))
         opts    (assoc opts :step-months (or (:step-months opts) (:test-months opts)))
         _       (validate-opts! opts)
         ms      (vec (sort (validate-months! (vec months-seq))))
         step    (:step-months opts)]
     (loop [i 0, acc (transient [])]
       (if (>= i (count ms))
         (persistent! acc)
         (let [split (split-at-index ms opts i)]
           (recur (+ i step)
                  (if split (conj! acc split) acc))))))))
