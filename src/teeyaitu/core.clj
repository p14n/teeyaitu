(ns teeyaitu.core)
(require '(clojure [string :as string]))
(require '(net.cgrand [enlive-html :as html]))
(use 'incanter.core)
(import [java.net URL])
(import java.util.Calendar)

;;For each stock examine the last 7 days for
;; ADX>30
;; 12 month high
;; outperfoming the FTSE by 15%

;;For this sublist find stocks
;; That have had a 2 month high/low between 3 and 7 days ago
;; Is now lower/higher that that value

;;For this sublist, find stocks that go through the previous days
;; high/low

(defn highest [days]
  (reduce #(if (> (%1 :high 0) (%2 :high)) %1 %2) {} days))

(defn lowest [days]
  (reduce #(if (< (%1 :low (%2 :low)) (%2 :low)) %1 %2) {} days))

(defn perf [then now]
  (* (with-precision 5 (/ (- (:close now) (:close then)) (:close then))) 100))

(defn date-int-to-cal [x]
  (let [year (int (/ x 10000))
        month (int (/ (- x (* year 10000)) 100))
        day (- x (* year 10000) (* month 100))
        cal (Calendar/getInstance)]
    (.set cal Calendar/YEAR year)
    (.set cal Calendar/MONTH month)
    (.set cal Calendar/DATE day)
    cal))


(defn in-last-7-days? [today x]
  (let [today-ms (.getTimeInMillis (date-int-to-cal today))
        x-ms (.getTimeInMillis (date-int-to-cal x))
        diff-days (/ (- today-ms x-ms) 86400000 )]
    (< diff-days 7)))

(defn outperforms? [x y by-amount]
  (cond (>= y 0) (> x (+ y by-amount))
        (< y 0) (< x (- y by-amount))))

(defn on-watchlist [day-index all-adxs ftse-3m-performance]
  (let [week-start (max (- day-index 5) 0)
        year-start (max (- day-index 240) 0)
        three-m-start (max (- day-index 60) 0)
        values-this-year (subvec all-adxs year-start day-index)
        year-high (highest values-this-year)
        year-low (lowest values-this-year)
        today (nth all-adxs day-index)
        perf-3m (perf (nth all-adxs three-m-start) today)
        highest-adx (reduce #(max %1 (:ADX %2)) 0 (subvec all-adxs week-start day-index ))]
    (cond (in-last-7-days? (:date today) (:date year-high)) #{:HIGH}
          (in-last-7-days? (:date today) (:date year-low)) #{:LOW}
          (outperforms? perf-3m ftse-3m-performance 15M) #{:OUTPERFORM}
          (> highest-adx 30) #{:ADX}
          (= 1 1) #{})))
