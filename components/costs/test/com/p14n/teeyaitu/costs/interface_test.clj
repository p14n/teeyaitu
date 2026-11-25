(ns com.p14n.teeyaitu.costs.interface-test
  (:require [clojure.test :refer [deftest is]]
            [com.p14n.teeyaitu.costs.interface :as costs])
  (:import (java.time LocalDate)))

(defn approx= [a b] (< (Math/abs (- (double a) (double b))) 1e-12))

(deftest spread-per-side
  (is (approx= 0.0015 (costs/trade-cost 1.0 costs/defaults)) "10 + 5 bps")
  (is (approx= 0.0015 (costs/trade-cost -1.0 costs/defaults)) "sells cost the same")
  (is (approx= 0.0 (costs/trade-cost 1.0 costs/zero-costs))))

(deftest funding
  (is (approx= (* 0.065 (/ 30 365.0)) (costs/funding-cost 1.0 30 0.04 costs/defaults))
      "long pays SONIA + fee")
  (is (approx= (- (* 0.005 (/ 30 365.0)) (* 0.015 (/ 30 365.0)))
         (costs/funding-cost -1.0 30 0.04 costs/defaults))
      "short receives SONIA - fee, pays borrow")
  (is (pos? (costs/funding-cost -1.0 30 0.01 costs/defaults))
      "short pays when SONIA is below the fee")
  (is (= 31 (costs/nights (LocalDate/parse "2024-01-31") (LocalDate/parse "2024-03-02")))
      "weekends and leap day count"))

(deftest boe-csv-and-lookup
  (let [rows (costs/parse-boe-csv "DATE,IUDSOIA\n04 Jan 2021,0.0461\n05 Jan 2021,0.0470\n2021-01-08,0.05\n")
        f    (costs/sonia-lookup (costs/sonia-series rows))]
    (is (= 3 (count rows)))
    (is (approx= 0.000461 (:rate (first rows))) "percent -> decimal")
    (is (approx= 0.00047 (f (LocalDate/parse "2021-01-07"))) "carries forward over gaps")
    (is (approx= 0.000461 (f (LocalDate/parse "2020-01-01"))) "before series start")))
