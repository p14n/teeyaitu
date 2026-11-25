(ns teeyaitu.trade-test
  (:use teeyaitu.core
        teeyaitu.adx
        midje.sweet
        clojure-csv.core)
  (import java.util.Calendar))

(defonce csv-3i (reverse (rest
                          (parse-csv (clojure.string/replace (slurp "test/3IN.L.csv") "-" "")))))
(def filtered-3i (filter #(not (= "000" (nth % 5))) csv-3i))
(def test-3i-vals (map #(-> {:date (Integer/parseInt (first %))
                             :high (to-bigdec (nth % 2))
                             :low (to-bigdec (nth % 3))
                             :close (to-bigdec (nth % 4))}) filtered-3i))

(def adxs-3i (reduce calc-adx [] test-3i-vals))



