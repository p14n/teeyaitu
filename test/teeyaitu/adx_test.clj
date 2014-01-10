(ns teeyaitu.adx-test
  (:use teeyaitu.adx
        midje.sweet
        clojure-csv.core
        ))

(def test-vals
  (parse-csv (clojure.string/replace
              (slurp "test/cs-adx.csv") "\r" "\n")))

(defn to-bigdec [x]
  (with-precision 10 :rounding HALF_UP (bigdec x)))

(def test-days
  (map #(-> {
             :high (to-bigdec (first %))
             :low (to-bigdec (second %))
             :close (to-bigdec (nth % 2))} )
       test-vals))

(def test-adxs (reduce calc-adx [] test-days))

(defn day [high low close]
  {:high high :low low :close close})

(def day-a1 (day 44.53M 43.98M 44.52M))
(def day-a2 (day 44.93M 44.36M 44.65M))
(def day-a3 (day 45.39M 44.70M 45.22M))

(def day-b1 (day 30.20M	29.41M 29.87M))
(def day-b2 (day 30.28M	29.32M 30.24M))
(def day-b3 (day 30.45M	29.96M 30.10M))
(def day-b4 (day 29.35M	28.74M	28.90M))


(fact "Calculates correct TR for two days 1"
      (let [calc (calc-tr-and-dm day-a1 day-a2)]
        (calc :TR)) => 0.57M)
(fact "Calculates correct TR for two days 2"
      (let [calc (calc-tr-and-dm day-b2 day-b3)]
        (calc :TR)) => 0.49M)
(fact "Calculates correct TR for two days 3"
      (let [calc (calc-tr-and-dm day-b3 day-b4)]
        (calc :TR)) => 1.36M)

(fact "Sums previous TR and DM values"
      (let [adxs [day-a1 (calc-tr-and-dm day-a1 day-a2)]
            summed (calc-tr14-initial adxs day-a3)]
        (summed :TR14)
        ) => 0.57M)

(fact "ADX value 27 should be 33.58"
      ((nth test-adxs 28) :ADX) => 33.58)


