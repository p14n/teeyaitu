(ns com.p14n.teeyaitu.price-store.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [tablecloth.api :as tc]
            [com.p14n.teeyaitu.price-store.interface :as store])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant LocalDate)))

(defn- row [ticker date close & {:keys [source fetched] :or {source :yahoo fetched "2026-01-01T00:00:00Z"}}]
  {:date (LocalDate/parse date) :ticker ticker :close close :adj-close close
   :volume nil :source source :fetched-at (Instant/parse fetched)})

(deftest unit-jump-detected
  (let [rows [(row "AAA" "2024-01-02" 1.50)
              (row "AAA" "2024-01-03" 151.0)   ; switched to pence
              (row "AAA" "2024-01-04" 152.0)
              (row "BBB" "2024-01-02" 2.0)
              (row "BBB" "2024-01-03" 2.1)]
        issues (store/quality-issues rows)]
    (is (= 1 (count issues)))
    (is (= {:ticker "AAA" :issue :unit-jump} (select-keys (first issues) [:ticker :issue])))
    (is (thrown? clojure.lang.ExceptionInfo (store/assert-quality! rows)))
    (is (= 2 (count (store/assert-quality! (filter #(= "BBB" (:ticker %)) rows)))))))

(deftest reverse-jump-and-normal-moves
  (is (= 1 (count (store/quality-issues [(row "A" "2024-01-02" 150.0) (row "A" "2024-01-03" 1.5)]))))
  (is (empty? (store/quality-issues [(row "A" "2024-01-02" 1.0) (row "A" "2024-01-03" 3.0)]))
      "a large but non-unit move is not flagged"))

(deftest parquet-round-trip
  (let [root (str (Files/createTempDirectory "price-store" (make-array FileAttribute 0)))
        first-load [(row "AAA" "2023-12-29" 1.0)
                    (row "AAA" "2024-01-02" 1.1)
                    (row "AAA" "2024-01-02" 1.2 :source :ig)]
        written (store/write-prices! root first-load)]
    (testing "partitioned by source and year"
      (is (= #{[:yahoo 2023] [:yahoo 2024] [:ig 2024]} (set (map (juxt :source :year) written)))))
    (testing "re-writing a row keeps the latest fetch"
      (store/write-prices! root [(row "AAA" "2024-01-02" 1.15 :fetched "2026-02-01T00:00:00Z")])
      (let [ds   (store/read-prices root {:sources #{:yahoo}})
            rows (tc/rows ds :as-maps)]
        (is (= 2 (tc/row-count ds)))
        (is (= 1.15 (:close (last rows))))
        (is (= (LocalDate/parse "2024-01-02") (:date (last rows))))
        (is (= :yahoo (:source (last rows))))
        (is (nil? (:volume (last rows))))
        (is (instance? Instant (:fetched-at (last rows))))))
    (testing "filters"
      (is (= 1 (tc/row-count (store/read-prices root {:sources #{:ig}}))))
      (is (= 2 (tc/row-count (store/read-prices root {:years #{2024}})))))))
