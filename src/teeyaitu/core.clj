(ns teeyaitu.core)
(require '(clojure [string :as string]))
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

(defn diff-in-days [today earlier]
  (let [today-ms (.getTimeInMillis (date-int-to-cal today))
        x-ms (.getTimeInMillis (date-int-to-cal earlier))]
    (/ (- today-ms x-ms) 86400000 )))

(defn in-last-7-days? [today x]
  (< (diff-in-days today x) 7))

(defn over-2-days? [today x]
  (> (diff-in-days today x) 2))


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
        highest-adx (reduce #(if (> (:ADX %1 0) (:ADX %2)) %1 %2) {} (subvec all-adxs week-start day-index ))]
    (cond (in-last-7-days? (:date today) (:date year-high)) #{:HIGH}
          (in-last-7-days? (:date today) (:date year-low)) #{:LOW}
          (outperforms? perf-3m ftse-3m-performance 15M)
           (if (> perf-3m 0) #{:OUTPERFORM-BULL} #{:OUTPERFORM-BEAR})
          (> (:ADX highest-adx) 30)
           (if (> (:+DI14 highest-adx) (:-DI14 highest-adx)) #{:ADX-BULL} #{:ADX-BEAR})
          :else #{})))

(defn recent-2m-high? [day-index all-adxs]
  (let [start-2m (max (- day-index 40) 0)
        adxs-2m (subvec all-adxs start-2m day-index)
        today (nth all-adxs day-index)
        high-2m (highest adxs-2m)
        low-2m (lowest adxs-2m)]
    (if (and (in-last-7-days? (:date today) (:date high-2m))
             (over-2-days? (:date today) (:date high-2m))
             (< (:close today) (:high high-2m)))
      true
      (if (and (in-last-7-days? (:date today) (:date low-2m))
             (over-2-days? (:date today) (:date low-2m))
             (> (:close today) (:low high-2m)))
      true
      false))))

;;Calculate adx for stock
;;step through each day, determine watchlist for the day
;;if watchlist, determine pullback setups
;;look at trades, stop if low<stop
;;look at yesterdays setups, trigger trades if high>entry

(defn find-setups [day-index stock-adxs ftse-3m-perf]
  (let [watchlist-val (on-watchlist day-index stock-adxs ftse-3m-perf)]
    (if (empty? watchlist-val)
      {}
      (if (recent-2m-high? stock-adxs day-index)
        {:watch watchlist-val :setup true}
        {:watch watchlist-val :setup false}))))

(defn add-watchlist-and-setups [watchlist stock-name range-of-days stock-adxs ftse-3m-vals]
  (let [merge-function
        #(let [today (nth stock-adxs %2)
               day-setup (find-setups %2 stock-adxs (ftse-3m-vals (:date today) 0))]
           (if (empty? day-setup)
             %1
             (-> %1
                 (assoc-in (:date today) :watch stock-name day-setup)
                 (assoc-in (:date today) :setup stock-name
                           (if (:setup day-setup) today {}))
                 )))]
    (reduce merge-function watchlist range-of-days)))

(defn faky-trady [stock-name stock-adxs ftse-3m-vals]
  (let [watchlist (add-watchlist-and-setups
                   (sorted-map {})
                   stock-name
                   (range 0 (count stock-adxs))
                   ftse-3m-vals)]
;;loop through the adxs and watchlist opening and closinfg trades 
    ))



