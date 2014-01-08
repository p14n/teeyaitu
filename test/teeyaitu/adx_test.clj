(ns teeyaitu.adx-test
  (:use teeyaitu.adx
        midje.sweet
        clojure-csv.core
        ))

(defonce test-vals
  (parse-csv (clojure.string/replace
              (slurp "test/cs-adx.csv") "\r" "\n")))

(def test-days
  (map #(-> {
             :high (first %)
             :low (second %)
             :close (nth % 2)} )
       test-vals))

(def test-adxs (map calc-adx [] test-days))

(defn day [high low close]
  {:high high :low low :close close})

(def day1 (day 44.53M 43.98M 44.52M))
(def day2 (day 44.93M 44.36M 44.65M))
(def day3 (day 45.39M 44.70M 45.22M))

(fact "Calculates correct TR for two days"
      (let [calc (calc-tr-and-dm day1 day2)]
        (calc :TR)) => 0.57M)

(fact "Sums previous TR and DM values"
      (let [adxs [day1 (calc-tr-and-dm day1 day2)]
            summed (calc-tr14-initial adxs day3)]
        (summed :TR14)
        ) => 0.57M)

(fact "ADX value 27 should be 33.58"
      ((nth test-adxs 27) :ADX) => 33.58)


