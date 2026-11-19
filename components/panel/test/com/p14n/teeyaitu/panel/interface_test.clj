(ns com.p14n.teeyaitu.panel.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [tablecloth.api :as tc]
            [com.p14n.teeyaitu.panel.interface :as panel])
  (:import (java.time DayOfWeek LocalDate YearMonth)
           (java.util Random)))

(defn weekdays [^LocalDate from n]
  (->> (iterate #(.plusDays ^LocalDate % 1) from)
       (remove #(#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (.getDayOfWeek ^LocalDate %)))
       (take n)))

(defn random-walk [ticker seed days]
  (let [rnd (Random. seed)]
    (map (fn [d p] {:ticker ticker :date d :close p :adj-close p})
         days
         (reductions (fn [p _] (* p (Math/exp (* 0.02 (.nextGaussian rnd))))) 10.0 (rest days)))))

(def days (weekdays (LocalDate/parse "2020-01-01") 700))
(def daily (concat (random-walk "AAA" 1 days) (random-walk "BBB" 2 days)))

(deftest one-row-per-ticker-month
  (let [rows (tc/rows (panel/monthly-panel daily) :as-maps)
        aaa  (filter #(= "AAA" (:ticker %)) rows)]
    (is (= (count (distinct (map #(YearMonth/from %) days))) (count aaa)))
    (is (= (LocalDate/parse "2020-01-31") (:date (first aaa))) "last trading day of Jan 2020")
    (is (nil? (:vol-60d (first aaa))) "not enough history yet")
    (is (number? (:vol-60d (last aaa))))
    (is (number? (:high-252d (last aaa))))))

(deftest rolling-vol-annualised
  (testing "constant growth has zero volatility"
    (let [px (take 70 (iterate #(* % 1.01) 1.0))]
      (is (< (last (panel/rolling-vol px 60)) 1e-9)))))

(deftest no-look-ahead-in-daily-stats
  ;; Shuffle every price after a month-end; stats at or before it must not change.
  (let [cutoff   (LocalDate/parse "2021-06-30")
        after?   #(.isAfter ^LocalDate (:date %) cutoff)
        rnd      (java.util.Random. 7)
        shuffled (let [pre  (remove after? daily)
                       post (vec (filter after? daily))
                       px   (let [l (java.util.ArrayList. ^java.util.Collection (map :close post))]
                              (java.util.Collections/shuffle l rnd) (vec l))]
                   (concat pre (map #(assoc %1 :close %2 :adj-close %2) post px)))
        upto     (fn [ds] (->> (tc/rows ds :as-maps)
                               (remove after?)
                               (sort-by (juxt :ticker :date))))]
    (is (= (upto (panel/monthly-panel daily)) (upto (panel/monthly-panel shuffled))))))
