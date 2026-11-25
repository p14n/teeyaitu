(ns com.p14n.teeyaitu.yahoo-client.interface-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [com.p14n.teeyaitu.yahoo-client.interface :as yahoo])
  (:import (java.time Instant LocalDate)))

;; Trimmed shape of a real v8 chart response. 1704189600 = 2024-01-02T10:00:00Z.
(def body
  (json/read-value
   "{\"chart\":{\"error\":null,\"result\":[{
      \"meta\":{\"currency\":\"GBp\",\"symbol\":\"BARC.L\"},
      \"timestamp\":[1704189600,1704276000,1704362400],
      \"indicators\":{
        \"quote\":[{\"close\":[150.5,null,152.0],\"volume\":[1000,2000,3000]}],
        \"adjclose\":[{\"adjclose\":[140.0,null,141.5]}]}}]}}"))

(def fetched (Instant/parse "2026-10-23T12:00:00Z"))

(deftest parses-pence-to-pounds
  (let [rows (yahoo/parse-chart "BARC" body fetched)]
    (is (= 2 (count rows)) "null closes are dropped")
    (is (= (LocalDate/parse "2024-01-02") (:date (first rows))))
    (is (< (Math/abs (- 1.505 (:close (first rows)))) 1e-12))
    (is (< (Math/abs (- 1.40 (:adj-close (first rows)))) 1e-12))
    (is (= :yahoo (:source (first rows))))
    (is (= fetched (:fetched-at (first rows))))))

(deftest gbp-not-scaled
  (let [b (assoc-in body ["chart" "result" 0 "meta" "currency"] "GBP")]
    (is (= 150.5 (:close (first (yahoo/parse-chart "BARC" b fetched)))))))

(deftest error-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (yahoo/parse-chart "X" {"chart" {"result" nil "error" {"code" "Not Found"}}} fetched))))

(deftest symbols
  (is (= "BARC.L" (yahoo/yahoo-symbol "BARC")))
  (is (= "BT-A.L" (yahoo/yahoo-symbol "BT.A"))))
