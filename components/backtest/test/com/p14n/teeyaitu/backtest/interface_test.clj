(ns com.p14n.teeyaitu.backtest.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.p14n.teeyaitu.backtest.interface :as bt]
            [com.p14n.teeyaitu.costs.interface :as costs])
  (:import (java.time YearMonth)
           (java.util Random)))

(defn- median [xs] (let [s (vec (sort xs)) n (count s)] (/ (+ (s (quot (dec n) 2)) (s (quot n 2))) 2.0)))

(defn fixture
  "100 stocks x 120 months of iid returns (mean 0.5%, sd 8%) with random vols, scores and
  12-1 momentum. :excess-1m is the true excess return (perfect foresight)."
  [seed]
  (let [rnd (Random. seed)]
    (vec
     (mapcat
      (fn [i]
        (let [m    (.plusMonths (YearMonth/of 2010 1) (long i))
              rows (vec (for [t (range 100)]
                          {:month m :date (.atEndOfMonth m) :ticker (format "T%03d" t)
                           :fwd-ret-1m (+ 0.005 (* 0.08 (.nextGaussian rnd)))
                           :vol-60d-raw (+ 0.15 (* 0.3 (.nextDouble rnd)))
                           :random (.nextDouble rnd)
                           :mom-12-1 (.nextDouble rnd)}))
              med  (median (map :fwd-ret-1m rows))]
          (map #(assoc % :excess-1m (- (:fwd-ret-1m %) med)) rows)))
      (range 120)))))

(def data (fixture 11))

(def plain {:weighting :equal :target-vol nil :max-weight nil :max-gross nil})
(def no-costs {:costs costs/zero-costs :sonia (costs/constant-sonia 0.0)})
(def with-costs {:costs costs/defaults :sonia (costs/constant-sonia 0.04)})

(defn- mean [xs] (/ (reduce + xs) (count xs)))

(deftest sanity-perfect-foresight
  (let [res (bt/run data (merge plain no-costs {:score-col :excess-1m}))
        m   (bt/metrics res)]
    (is (= 120 (count res)))
    (is (> (:annual-return m) 1.0) "perfect foresight must look implausibly good")
    (is (zero? (:cost-drag m)))))

(deftest sanity-random-scores-match-equal-weight
  (let [rnd (bt/run data (merge plain with-costs {:score-col :random}))
        ew  (bt/run data (merge plain with-costs {:select :all}))]
    (testing "gross returns are close"
      (is (< (Math/abs (- (mean (map :gross rnd)) (mean (map :gross ew)))) 0.005)))
    (testing "net = gross - costs"
      (doseq [r rnd]
        (is (< (Math/abs (- (:net r) (- (:gross r) (:trade-cost r) (:funding r)))) 1e-12))))
    (testing "random selection trades more, so pays more"
      (is (> (mean (map :trade-cost rnd)) (mean (map :trade-cost ew)))))))

(deftest sanity-doubling-spread
  (let [base  {:costs {:spread-bps 10 :slippage-bps 0 :funding-fee 0.0 :borrow-rate 0.0}
               :sonia (costs/constant-sonia 0.0) :score-col :random}
        r10   (bt/run data (merge plain base))
        r20   (bt/run data (merge plain base {:costs (assoc (:costs base) :spread-bps 20)}))
        drop  (- (mean (map :net r10)) (mean (map :net r20)))
        expect (* (mean (map :traded r10)) (/ 10 10000.0))]
    (is (pos? drop))
    (is (< (Math/abs (- drop expect)) (* 0.05 expect))
        "net falls by about traded notional x extra spread")))

(deftest weights-and-caps
  (let [rows (filter #(= (YearMonth/of 2010 1) (:month %)) data)
        w    (bt/target-weights rows {:score-col :random})]
    (is (= 20 (count w)) "top quintile")
    (is (every? #(<= % 0.1) (vals w)) "10% cap")
    (is (<= (reduce + (vals w)) 1.0) "gross cap"))
  (let [rows [{:ticker "A" :random 1.0 :vol-60d-raw 0.1} {:ticker "B" :random 0.9 :vol-60d-raw 0.2}]
        w    (bt/target-weights rows {:score-col :random :select :all :target-vol nil :max-weight nil})]
    (is (< (Math/abs (- (w "A") (/ 2 3.0))) 1e-12) "inverse vol")))

(deftest staggered-sleeves
  (let [res (bt/run data (merge plain with-costs {:score-col :random :horizon 3}))]
    (is (= 120 (count res)))
    (is (< (Math/abs (- (/ 1 3.0) (:gross-exposure (first res)))) 1e-9)
        "first month only one of three sleeves is invested")
    (is (< (mean (map :turnover (drop 3 res)))
           (mean (map :turnover (drop 3 (bt/run data (merge plain with-costs {:score-col :random}))))))
        "a third of the book turns over each month")))

(deftest drawdown-and-gate
  (is (< (Math/abs (- 0.5 (bt/max-drawdown [0.1 -0.5 0.2]))) 1e-12))
  (is (bt/promote? {:sharpe 0.8 :max-drawdown 0.25} {:sharpe 0.6 :max-drawdown 0.2}))
  (is (not (bt/promote? {:sharpe 0.8 :max-drawdown 0.26} {:sharpe 0.6 :max-drawdown 0.2})))
  (is (not (bt/promote? {:sharpe 0.5 :max-drawdown 0.1} {:sharpe 0.6 :max-drawdown 0.2}))))

(deftest comparison-report
  (let [rep (bt/compare-strategies data (merge with-costs {:score-col :random
                                                           :warnings ["SURVIVORSHIP BIAS: test" nil]}))]
    (is (= ["SURVIVORSHIP BIAS: test"] (:warnings rep)))
    (is (every? #(get-in rep [% :overall :sharpe]) [:model :baseline :equal-weight]))
    (is (number? (:model-beat-baseline-share rep)))
    (is (not (contains? (:config rep) :sonia)))))
