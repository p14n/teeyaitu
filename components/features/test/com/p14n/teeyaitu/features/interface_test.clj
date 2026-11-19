(ns com.p14n.teeyaitu.features.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [tablecloth.api :as tc]
            [com.p14n.teeyaitu.features.interface :as features])
  (:import (java.time YearMonth)
           (java.util ArrayList Collections Random)))

(def start (YearMonth/of 2015 1))
(def n-months 40)

(defn monthly-fixture
  "Synthetic monthly panel: 20 tickers x 40 months of random-walk prices."
  [seed]
  (let [rnd (Random. seed)]
    (vec
     (for [t (range 20)
           :let [ticker (format "T%02d" t)
                 prices (reductions (fn [p _] (* p (Math/exp (* 0.08 (.nextGaussian rnd)))))
                                    10.0 (range (dec n-months)))]
           [i p] (map-indexed vector prices)
           :let [m (.plusMonths ^YearMonth start (long i))]]
       {:ticker ticker :month m :date (.atEndOfMonth m)
        :close p :adj-close p
        :vol-60d (+ 0.15 (* 0.3 (.nextDouble rnd)))
        :high-252d (* p (+ 1.0 (* 0.3 (.nextDouble rnd))))}))))

(def panel (monthly-fixture 42))

(defn- feature-view [ds ^YearMonth upto]
  (->> (tc/rows ds :as-maps)
       (remove #(.isAfter ^YearMonth (:month %) upto))
       (map #(select-keys % (concat [:ticker :month] features/raw-feature-cols)))
       (sort-by (juxt :month :ticker))))

(deftest builds-ranked-features-and-targets
  (let [ds   (features/build panel)
        rows (tc/rows ds :as-maps)]
    (is (= (.plusMonths ^YearMonth start 12) (:month (first rows))) "needs 13 months of history")
    (is (every? #(<= 0.0 (:mom-12-1 %) 1.0) rows) "features are ranks")
    (is (every? #(nil? (:fwd-ret-1m %)) (filter #(= (.plusMonths ^YearMonth start (dec n-months)) (:month %)) rows))
        "latest month has no forward return but is kept for scoring")
    (testing "excess is relative to the month's median"
      (let [m (filter #(= (.plusMonths ^YearMonth start 20) (:month %)) rows)]
        (is (= 10 (count (filter #(= 1 (:label-1m %)) m))))))))

(deftest leakage-shuffle-future-prices
  ;; Shuffle every price after month t: every feature at or before t is unchanged.
  (let [t        (YearMonth/of 2017 6)
        after?   #(.isAfter ^YearMonth (:month %) t)
        post     (filter after? panel)
        shuffled (let [l (ArrayList. ^java.util.Collection (map :adj-close post))]
                   (Collections/shuffle l (Random. 3))
                   (concat (remove after? panel)
                           (map #(assoc %1 :adj-close %2 :close %2) post (vec l))))]
    (is (= (feature-view (features/build panel) t)
           (feature-view (features/build shuffled) t)))))

(deftest leakage-no-target-in-feature-list
  (is (not-any? #(re-find #"fwd|excess|label" (name %)) features/model-feature-cols))
  (is (thrown? clojure.lang.ExceptionInfo
               (features/assert-no-leaky-features! [:ret-1m :excess-1m]))))

(deftest leakage-ranks-are-per-month
  (let [rows    (filter features/complete-history? (features/add-time-series panel))
        m1      (YearMonth/of 2017 3)
        m2      (YearMonth/of 2017 4)
        view    (fn [rs] (->> rs (filter #(= m1 (:month %)))
                              (map #(select-keys % (cons :ticker features/raw-feature-cols)))
                              (sort-by :ticker)))]
    (is (= (view (features/rank-features rows))
           (view (features/rank-features (remove #(= m2 (:month %)) rows)))))))

(deftest percentile-rank-ties-and-nils
  (is (= [0.0 0.625 0.625 nil 0.25 1.0] (features/percentile-ranks [1 5 5 nil 3 9])))
  (is (= [0.5] (features/percentile-ranks [7]))))

(deftest membership-filter
  (let [ds (features/build panel {:member? (fn [ticker _] (not= "T00" ticker))})]
    (is (not-any? #(= "T00" %) (tc/column ds :ticker)))))
