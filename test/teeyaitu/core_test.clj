(ns teeyaitu.core-test
  (:use teeyaitu.core
        teeyaitu.adx
        midje.sweet
        clojure-csv.core)
  (import java.util.Calendar))

(defonce test-vals
  (parse-csv (clojure.string/replace
              (slurp "test/cs-adx.csv") "\r" "\n")))

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

(fact "Can add trade"
      (add-trade 130.0M 150.0M)
      (first @trades) => {:closed false :long false :open 130.0M :stop  150.0M})

(fact "Can calculate open profit on short"
      (calc-open-profit {:low 120.0M} {:closed false :long false :open 130.0M :stop 150.0M})
       => {:closed false :long false :open 130.0M :stop 150.0M
           :profit 10.0M :max-profit 10.0M})

(fact "Can calculate open profit on long"
      (calc-open-profit {:high 170.0M} {:closed false :long true :open 150.0M :stop 130.0M})
       => {:closed false :long true :open 150.0M :stop 130.0M
           :profit 20.0M :max-profit 20.0M})

(fact "Does not close trade that has not hit the stop"
      (close-stopped-trade {:low 131.0M} {:closed false :long true :open 150.0M :stop 130.0M})       => {:closed false :long true :open 150.0M :stop 130.0M})

(fact "Closes long trade that has hit the stop"
      (close-stopped-trade {:low 129.0M} {:closed false :long true :open 150.0M :stop 130.0M})       => {:closed true :long true :open 150.0M :stop 130.0M :profit -20.0M})

(fact "Closes short trade that has hit the stop"
      (close-stopped-trade {:high 170.0M} {:closed false :long false :open 150.0M :stop 170.0M})       => {:closed true :long false :open 150.0M :stop 170.0M :profit -20.0M})
