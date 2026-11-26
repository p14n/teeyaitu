(ns com.p14n.teeyaitu.model.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.p14n.teeyaitu.model.interface :as model])
  (:import (java.time YearMonth)))

(defn dist
  "Months from a to b (positive when b is later)."
  [a b] (.until ^YearMonth a ^YearMonth b java.time.temporal.ChronoUnit/MONTHS))

(defn months-from
  "n consecutive YearMonths starting at start."
  [start n] (vec (for [i (range n)] (.plusMonths ^YearMonth start (long i)))))

(defn months
  "n consecutive YearMonths starting at 2000-01."
  ([n] (months n 2000 1))
  ([n y m] (months-from (YearMonth/of y m) n)))

(def small {:train-months 12 :test-months 3 :purge-months 3 :label-horizon 3})

(deftest walk-forward-shape
  (let [ms  (months 24)
        sps (model/walk-forward ms small)]
    (testing "window rolls forward by :step-months (defaults to :test-months)"
      (is (= 3 (count sps)))
      (is (every? #(and (seq (:train-months %)) (seq (:test-months %))) sps)))
    (testing "train windows are fixed-size and tests advance by one step"
      (is (every? #(= 12 (count (:train-months %))) sps))
      (is (= (months-from (YearMonth/of 2001 4) 3) (:test-months (first sps))))
      (is (= (months-from (YearMonth/of 2001 7) 3) (:test-months (second sps))))
      (is (= (months-from (YearMonth/of 2001 10) 3) (:test-months (last sps)))))))

(deftest purge-gap-honoured
  (let [ms  (months 24)
        sps (model/walk-forward ms small)]
    (testing "no train month's label window (month + horizon) touches a test month"
      (doseq [{:keys [train-months test-months]} sps
              t train-months
              h (range 1 (inc (:label-horizon small)))
              m test-months]
        (is (neg? (.compareTo (.plusMonths t (long h)) m))
            (str "label of " t " (+" h "m) leaks into test month " m))))
    (testing "the gap is exactly the purge width between train end and test start"
      (doseq [{:keys [train-months test-months]} sps]
        (is (zero? (- (dist (last train-months) (first test-months))
                      (:purge-months small)
                      1)))))))

(deftest order-and-coverage
  (let [ms  (months 24)
        sps (model/walk-forward ms small)]
    (testing "months within each split are ascending"
      (doseq [{:keys [train-months test-months]} sps]
        (is (= train-months (sort train-months)))
        (is (= test-months (sort test-months)))))
    (testing "every train month precedes the split's first test month"
      (doseq [{:keys [train-months test-months]} sps]
        (is (every? #(pos? (dist % (first test-months))) train-months))))
    (testing "input order does not matter"
      (is (= sps (model/walk-forward (reverse ms) small))))
    (testing "every month from the first test start onward is tested exactly once"
      (let [tested (mapcat :test-months sps)]
        (is (= (months-from (YearMonth/of 2001 4) 9) tested))))))

(deftest configurable-step-and-gap
  (let [sps (model/walk-forward (months 30)
                                {:train-months 12 :test-months 3 :step-months 6
                                 :purge-months 6 :label-horizon 3})]
    (testing "a wider purge pushes the first test start later"
      (is (= (months-from (YearMonth/of 2001 7) 3) (:test-months (first sps)))))
    (testing "step larger than test window leaves months between folds untested"
      (is (= 2 (count sps)))
      (is (= (months-from (YearMonth/of 2002 1) 3) (:test-months (second sps)))))))

(deftest purge-must-cover-label-horizon
  (testing "purge narrower than the label horizon is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"purge"
                          (model/walk-forward (months 24)
                                              {:train-months 12 :test-months 3
                                               :purge-months 2 :label-horizon 3}))))
  (testing "purge defaults to the label horizon"
    (let [sps (model/walk-forward (months 24) {:train-months 12 :test-months 3 :label-horizon 3})]
      (is (= 3 (count sps)))
      (is (= (months-from (YearMonth/of 2001 4) 3) (:test-months (first sps)))))))

(deftest degenerate-inputs
  (testing "no months -> no splits"
    (is (= [] (model/walk-forward [] small))))
  (testing "too few months for even one split -> no splits"
    (is (= [] (model/walk-forward (months 15) small)))
    (is (= [] (model/walk-forward (months 15) (assoc small :train-months 14)))))
  (testing "exactly enough for one split"
    (is (= 1 (count (model/walk-forward (months 18) small)))))
  (testing "non-positive windows are rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"train"
                          (model/walk-forward (months 24) (assoc small :train-months 0))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"test"
                          (model/walk-forward (months 24) (assoc small :test-months 0))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"step"
                          (model/walk-forward (months 24) (assoc small :step-months -1)))))
  (testing "duplicate months are rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"duplicate"
                          (model/walk-forward (concat (months 18) [(first (months 1))]) small))))
  (testing "an incomplete final test window is dropped"
    (is (= 1 (count (model/walk-forward (months 20) small))))))
