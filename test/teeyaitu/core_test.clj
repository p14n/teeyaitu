(ns teeyaitu.core-test
  (:use teeyaitu.core
        teeyaitu.adx
        midje.sweet
        clojure-csv.core)
  (import java.util.Calendar))


(defonce test-vals
  (parse-csv (clojure.string/replace
              (slurp "test/cs-adx.csv") "\r" "\n")))

(defn to-bigdec [x]
  (with-precision 10 :rounding HALF_UP (bigdec x)))

(def cal (Calendar/getInstance))

(defn add-day [] (do (.add cal Calendar/DATE 1) cal))

(defn cal-to-int [x]
  (let [year (.get cal Calendar/YEAR)
        month (+ 1 (.get cal Calendar/MONTH))
        day (.get cal Calendar/DATE)
        ]
    (- (+ (* year 10000) (* month 100) day) 20000000)))

(def test-days
  (map #(-> {
             :high (to-bigdec (first %))
             :low (to-bigdec (second %))
             :close (to-bigdec (nth % 2))
             :date (cal-to-int (add-day))
             }
            )
       test-vals))

(def test-adxs (reduce calc-adx [] test-days))

(fact "Gets highest"
      (let [high {:high 3}
            mid {:high 2}
            low {:high 1}]
        (:high (highest [low high mid]))) => 3)

(fact "Gets lowest"
      (let [high {:low 3}
            mid {:low 2}
            low {:low 1}]
        (:low (lowest [low high mid]))) => 1)

(fact "Performance calculated"
      (perf {:close 100M} {:close 125M}) => 25.00M)

(fact "Can spot date in last 7 days"
      (in-last-7-days? 140102 131227) => true)

(fact "Can spot date outside last 7 days"
      (in-last-7-days? 140108 140101) => false)

(fact "Can spot outperform"
      (outperforms? 35 23 10) => true)

(fact "Can spot underperform"
      (outperforms? 40 35 10) => false)

(fact "Can spot negative outperform"
      (outperforms? -35 -23 10) => true)

(fact "Can spot negative underperform"
      (outperforms? -40 -35 10) => false)


