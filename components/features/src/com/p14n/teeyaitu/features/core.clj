(ns com.p14n.teeyaitu.features.core
  "Momentum features, cross-sectional ranks and forward targets over the monthly panel.
  Features at month t use only prices dated t or earlier; targets use prices after t."
  (:require [clojure.string :as str]
            [tablecloth.api :as tc])
  (:import (java.time YearMonth)))

(def raw-feature-cols
  [:ret-1m :ret-3m :ret-6m :mom-12-1 :vol-60d
   :ret-3m-scaled :ret-6m-scaled :mom-12-1-scaled :dist-52w-high])

(def model-feature-cols
  "Columns the model may train on. All are cross-sectional percentile ranks."
  [:ret-1m :ret-3m-scaled :ret-6m-scaled :mom-12-1-scaled :vol-60d :dist-52w-high])

(def target-cols [:fwd-ret-1m :fwd-ret-3m :excess-1m :excess-3m :label-1m :label-3m])

(defn ->rows [data]
  (if (tc/dataset? data) (tc/rows data :as-maps) data))

(defn leaky-column? [col]
  (let [n (name col)]
    (or (str/includes? n "fwd") (str/includes? n "excess") (str/includes? n "label"))))

(defn assert-no-leaky-features! [cols]
  (when-let [bad (seq (filter leaky-column? cols))]
    (throw (ex-info "Target columns in feature list" {:columns (vec bad)})))
  cols)

;; --- per-ticker time-series features -----------------------------------------------

(defn- ret [now then]
  (when (and now then (pos? (double then))) (- (/ (double now) (double then)) 1.0)))

(defn- scaled [x vol]
  (when (and x vol (pos? (double vol))) (/ (double x) (double vol))))

(defn- ticker-features [rows]
  (let [px (into {} (map (juxt :month :adj-close)) rows)
        p  (fn [^YearMonth m k] (px (.minusMonths m (long k))))
        f  (fn [^YearMonth m k] (px (.plusMonths m (long k))))]
    (for [r rows
          :let [m    (:month r)
                p0   (:adj-close r)
                vol  (:vol-60d r)
                r3   (ret p0 (p m 3))
                r6   (ret p0 (p m 6))
                m121 (ret (p m 1) (p m 12))]]
      (assoc r
             :ret-1m          (ret p0 (p m 1))
             :ret-3m          r3
             :ret-6m          r6
             :mom-12-1        m121
             :ret-3m-scaled   (scaled r3 vol)
             :ret-6m-scaled   (scaled r6 vol)
             :mom-12-1-scaled (scaled m121 vol)
             :dist-52w-high   (when (:high-252d r) (ret p0 (:high-252d r)))
             :fwd-ret-1m      (ret (f m 1) p0)
             :fwd-ret-3m      (ret (f m 3) p0)))))

(defn add-time-series
  "Adds raw features and forward returns per ticker, looking up prices by calendar month
  so a missing month yields nil rather than a mis-aligned lag."
  [monthly]
  (->> (->rows monthly) (group-by :ticker) vals (mapcat ticker-features)))

(defn complete-history?
  "True when every raw feature is present (implies 13 months of history)."
  [r]
  (every? #(some? (get r %)) raw-feature-cols))

;; --- cross-sectional ranks and targets ------------------------------------------------

(defn percentile-ranks
  "Percentile ranks in [0, 1] with ties averaged; nils stay nil; a single value gets 0.5."
  [xs]
  (let [xs      (vec xs)
        present (keep-indexed (fn [i x] (when (some? x) [i (double x)])) xs)
        n       (count present)
        out     (transient (vec (repeat (count xs) nil)))]
    (if (zero? n)
      xs
      (let [sorted (vec (sort-by second present))]
        (loop [i 0 out out]
          (if (>= i n)
            (persistent! out)
            (let [v (second (sorted i))
                  j (loop [j i] (if (and (< (inc j) n) (= v (second (sorted (inc j))))) (recur (inc j)) j))
                  rank (if (= n 1) 0.5 (/ (/ (+ i j) 2.0) (dec n)))
                  out  (reduce (fn [o k] (assoc! o (first (sorted k)) rank)) out (range i (inc j)))]
              (recur (inc j) out))))))))

(defn- median [xs]
  (let [s (vec (sort xs)) n (count s)]
    (when (pos? n)
      (if (odd? n) (s (quot n 2)) (/ (+ (s (dec (quot n 2))) (s (quot n 2))) 2.0)))))

(defn- rank-month [rows]
  (let [rows   (vec rows)
        ranked (reduce (fn [rs col]
                         (let [rk (percentile-ranks (map col rs))]
                           (mapv #(assoc %1 col %2) rs rk)))
                       (mapv #(assoc % :vol-60d-raw (:vol-60d %) :mom-12-1-raw (:mom-12-1 %)) rows)
                       raw-feature-cols)]
    ranked))

(defn rank-features
  "Replaces each raw feature with its percentile rank within its month. Keeps
  :vol-60d-raw (portfolio weighting) and :mom-12-1-raw (reporting)."
  [rows]
  (->> (->rows rows) (group-by :month) vals (mapcat rank-month)))

(defn- excess-month [rows]
  (let [rows (vec rows)
        med1 (median (keep :fwd-ret-1m rows))
        med3 (median (keep :fwd-ret-3m rows))
        ex   (fn [x med] (when (and x med) (- x med)))
        lbl  (fn [x] (when x (if (pos? x) 1 0)))]
    (map (fn [r]
           (let [e1 (ex (:fwd-ret-1m r) med1)
                 e3 (ex (:fwd-ret-3m r) med3)]
             (assoc r :excess-1m e1 :excess-3m e3 :label-1m (lbl e1) :label-3m (lbl e3))))
         rows)))

(defn add-targets
  "Excess forward return versus the month's cross-sectional median, and 0/1 labels.
  Rows with no forward price keep nil targets (dropped for training, kept for scoring)."
  [rows]
  (->> (->rows rows) (group-by :month) vals (mapcat excess-month)))

(defn build
  "monthly panel -> model-ready dataset. opts:
    :member? (fn [ticker date] bool) index membership on the month-end date; default all."
  ([monthly] (build monthly {}))
  ([monthly {:keys [member?] :or {member? (constantly true)}}]
   (->> (add-time-series monthly)
        (filter complete-history?)
        (filter #(member? (:ticker %) (:date %)))
        rank-features
        add-targets
        (sort-by (juxt :month :ticker))
        (#(tc/dataset % {:dataset-name "features"})))))

(defn training-rows
  "Rows with a non-nil target, for fitting."
  [ds target]
  (tc/select-rows ds (fn [r] (some? (get r target)))))
