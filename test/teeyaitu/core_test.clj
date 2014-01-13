(ns teeyaitu.core-test
  (:use teeyaitu.core
        teeyaitu.adx
        midje.sweet
        clojure-csv.core))

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


