(ns com.p14n.teeyaitu.costs.core
  "Spread-bet cost model. All amounts are fractions of capital (notional as a weight)."
  (:require [clojure.string :as str])
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)
           (java.time.temporal ChronoUnit)
           (java.util Locale TreeMap)))

(def defaults {:spread-bps 10 :slippage-bps 5 :funding-fee 0.025 :borrow-rate 0.005})

(def zero-costs {:spread-bps 0 :slippage-bps 0 :funding-fee 0.0 :borrow-rate 0.0})

(defn trade-cost
  "Half-spread plus slippage on traded notional, per side. :spread-bps is the per-side
  cost (half the bid-ask spread)."
  [traded-notional {:keys [spread-bps slippage-bps] :or {spread-bps 0 slippage-bps 0}}]
  (* (Math/abs (double traded-notional)) (/ (+ (double spread-bps) (double slippage-bps)) 10000.0)))

(defn funding-cost
  "Overnight funding for a signed notional held `nights` calendar nights at annual `sonia`
  (decimal, e.g. 0.05). Positive = cost, negative = credit.
  Longs pay SONIA + fee; shorts receive SONIA - fee and pay :borrow-rate."
  [notional nights sonia {:keys [funding-fee borrow-rate] :or {funding-fee 0.0 borrow-rate 0.0}}]
  (let [n  (double notional)
        a  (Math/abs n)
        yr (/ (double nights) 365.0)
        s  (double sonia)]
    (cond (pos? n) (* a (+ s funding-fee) yr)
          (neg? n) (- (* a borrow-rate yr) (* a (- s funding-fee) yr))
          :else    0.0)))

(defn nights
  "Calendar nights between two dates (weekends count)."
  [^LocalDate from ^LocalDate to]
  (.between ChronoUnit/DAYS from to))

;; --- SONIA ------------------------------------------------------------------------

(defn sonia-series
  "rows of {:date LocalDate :rate decimal} -> sorted series."
  [rows]
  (let [m (TreeMap.)]
    (doseq [{:keys [date rate]} rows] (.put m date (double rate)))
    m))

(defn sonia-lookup
  "fn of date -> latest rate on or before date (earliest rate for dates before the series)."
  [^TreeMap series]
  (when (.isEmpty series) (throw (ex-info "Empty SONIA series" {})))
  (fn [date]
    (if-let [e (.floorEntry series date)]
      (.getValue e)
      (.getValue (.firstEntry series)))))

(defn constant-sonia [rate] (fn [_] (double rate)))

(def ^:private boe-date (DateTimeFormatter/ofPattern "dd MMM yyyy" Locale/ENGLISH))

(defn- parse-date [s]
  (let [s (str/trim s)]
    (if (re-matches #"\d{4}-\d{2}-\d{2}" s)
      (LocalDate/parse s)
      (LocalDate/parse s boe-date))))

(defn parse-boe-csv
  "Parses a Bank of England database CSV export (DATE,IUDSOIA) with rates in percent,
  e.g. \"04 Jan 2021,0.0461\". ISO dates are also accepted. Returns {:date :rate} rows."
  [text]
  (->> (str/split-lines text)
       (map str/trim)
       (remove str/blank?)
       (keep (fn [line]
               (let [[d r] (map #(str/replace % "\"" "") (str/split line #","))]
                 (when (and d r (re-find #"\d" d) (re-matches #"-?[\d.]+" (str/trim r)))
                   {:date (parse-date d) :rate (/ (Double/parseDouble (str/trim r)) 100.0)}))))
       (sort-by :date)
       vec))
