(ns com.p14n.teeyaitu.universe.interface-test
  (:require [clojure.test :refer [deftest is]]
            [com.p14n.teeyaitu.universe.interface :as universe])
  (:import (java.time LocalDate)))

(def d #(LocalDate/parse %))

(def fixture
  (universe/normalise
   {:index "TEST" :as-of "2020-01-01" :point-in-time? true
    :members [{:ticker "AAA" :from nil :to nil}
              {:ticker "BBB" :from "2010-01-01" :to "2015-01-01"}
              {:ticker "BBB" :from "2018-01-01" :to nil}]}))

(deftest membership-windows
  (is (universe/member? fixture "AAA" (d "1990-01-01")))
  (is (not (universe/member? fixture "BBB" (d "2009-12-31"))))
  (is (universe/member? fixture "BBB" (d "2010-01-01")) ":from inclusive")
  (is (not (universe/member? fixture "BBB" (d "2015-01-01"))) ":to exclusive")
  (is (universe/member? fixture "BBB" (d "2019-06-30")) "re-entry")
  (is (not (universe/member? fixture "ZZZ" (d "2019-06-30"))))
  (is (= ["AAA"] (universe/members-on fixture (d "2016-01-01")))))

(deftest bundled-ftse100
  (let [m (universe/load-membership)]
    (is (= 100 (count (universe/tickers m))))
    (is (universe/survivorship-biased? m))
    (is (re-find #"SURVIVORSHIP" (universe/survivorship-warning m)))
    (is (nil? (universe/survivorship-warning fixture)))))
