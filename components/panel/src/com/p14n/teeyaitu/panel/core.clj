(ns com.p14n.teeyaitu.panel.core
  "Daily prices -> one row per ticker per month-end. Daily statistics (volatility,
  52-week high) are computed before resampling, from data dated on or before each day."
  (:require [tablecloth.api :as tc])
  (:import (java.time LocalDate YearMonth)))

(defn ->rows [data]
  (if (tc/dataset? data) (tc/rows data :as-maps) data))

(defn- price [r] (double (or (:adj-close r) (:close r))))

(defn- std [xs]
  (let [n (count xs)]
    (when (> n 1)
      (let [m (/ (reduce + xs) n)]
        (Math/sqrt (/ (reduce + (map #(let [d (- % m)] (* d d)) xs)) (dec n)))))))

(defn rolling-vol
  "Annualised std of daily log returns over the trailing `window` returns ending at each
  index (inclusive); nil until a full window is available."
  [prices window]
  (let [prices (vec prices)
        n      (count prices)
        lr     (vec (cons nil (map (fn [a b] (Math/log (/ b a))) prices (rest prices))))]
    (mapv (fn [i]
            (when (>= i window)
              (* (std (subvec lr (inc (- i window)) (inc i))) (Math/sqrt 252.0))))
          (range n))))

(defn rolling-max
  "Max of the trailing `window` values ending at each index (inclusive); nil until full."
  [xs window]
  (let [xs (vec xs)]
    (mapv (fn [i]
            (when (>= i (dec window))
              (reduce max (subvec xs (inc (- i window)) (inc i)))))
          (range (count xs)))))

(defn add-daily-stats
  "Adds :vol-60d and :high-252d to each daily row, per ticker."
  [daily {:keys [vol-window high-window] :or {vol-window 60 high-window 252}}]
  (->> (->rows daily)
       (group-by :ticker)
       vals
       (mapcat (fn [rs]
                 (let [rs   (vec (sort-by :date rs))
                       px   (mapv price rs)
                       vol  (rolling-vol px vol-window)
                       high (rolling-max px high-window)]
                   (map (fn [r v h] (assoc r :adj-close (price r) :vol-60d v :high-252d h))
                        rs vol high))))))

(defn month-end
  "Keeps the last trading day of each month per ticker, adding :month (YearMonth)."
  [daily]
  (->> (->rows daily)
       (group-by (fn [r] [(:ticker r) (YearMonth/from (:date r))]))
       (map (fn [[[_ m] rs]]
              (-> (apply max-key #(.toEpochDay ^LocalDate (:date %)) rs)
                  (assoc :month m))))
       (map #(select-keys % [:ticker :month :date :close :adj-close :vol-60d :high-252d]))
       (sort-by (juxt :month :ticker))
       vec))

(defn monthly-panel
  ([daily] (monthly-panel daily {}))
  ([daily opts]
   (tc/dataset (month-end (add-daily-stats daily opts)) {:dataset-name "monthly-panel"})))
