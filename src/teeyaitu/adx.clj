(ns teeyaitu.adx)
(import java.math.RoundingMode)

(def adx-vals {:day 131201
               :high 0.0
               :low 0.0
               :close 0.0
               :TR 0.0
               :+DM 0.0
               :-DM 0.0
               :TR14 0.0
               :+DM14 0.0
               :-DM14 0.0
               :+DI14 0.0
               :-DI14 0.0
               :DX 0.0
               :ADX 0.0})

(defn abs [x]
  (if (< x 0) (* x -1) x))

(defn div [x y] (with-precision 10 (/ x y)))

(defn dp [scale num] "Decimal points"
  (.setScale (bigdec num) scale RoundingMode/HALF_DOWN))

(defn calc-tr [curr-hi curr-lo prev-close]
  (let [hi-less-lo (- curr-hi curr-lo)
        hi-less-close (abs (- curr-hi prev-close))
        lo-less-close (abs (- curr-lo prev-close))]
    (max (max hi-less-lo hi-less-close) lo-less-close)))

(defn calc-plus-dm [prev-hi prev-lo curr-hi curr-lo]
  (let [curr-hi-less-prev-hi (- curr-hi prev-hi)
        prev-low-less-curr-low (- prev-lo curr-lo)]
    (if (and (> curr-hi-less-prev-hi 0)
             (> curr-hi-less-prev-hi prev-low-less-curr-low))
      curr-hi-less-prev-hi
      0)))

(defn calc-minus-dm [prev-hi prev-lo curr-hi curr-lo]
  (let [curr-hi-less-prev-hi (- curr-hi prev-hi)
        prev-low-less-curr-low (- prev-lo curr-lo)]
    (if (and (> prev-low-less-curr-low 0)
             (> prev-low-less-curr-low curr-hi-less-prev-hi))
      prev-low-less-curr-low
      0)))

(defn calc-tr-and-dm [prev current]
  (-> current
      (assoc :TR (calc-tr (current :high) (current :low) (prev :close)))
      (assoc :+DM (calc-plus-dm (prev :high) (prev :low) (current :high) (current :low)))
      (assoc :-DM (calc-minus-dm (prev :high) (prev :low) (current :high) (current :low)))))

(defn calc-di-and-dx [curr]
  (let [plus-di14 (dp 2 (* 100 (div (curr :+DM14) (curr :TR14))))
        minus-di14 (dp 2 (* 100 (div (curr :-DM14) (curr :TR14))))
        di14-diff (abs (- plus-di14 minus-di14))
        di14-sum (+ plus-di14 minus-di14)]
    (-> curr
        (assoc :+DI14 plus-di14)
        (assoc :-DI14 minus-di14)        
        (assoc :DX (dp 2 (div (* 100 di14-diff) di14-sum))))))

(defn sum-tr-and-dm [summed nextday]
    (-> summed
        (assoc :TR14 (+ (summed :TR14 0) (nextday :TR 0)))
        (assoc :+DM14 (+ (summed :+DM14 0) (nextday :+DM 0)))
        (assoc :-DM14 (+ (summed :-DM14 0) (nextday :-DM 0)))
        ))

(defn calc-tr14-initial [adxs new-day]
  "To be used to calculate the initial value of TR14"
  (let [initial-val (-> new-day
                        (assoc :TR14 (new-day :TR 0))
                        (assoc :+DM14 (new-day :+DM 0))
                        (assoc :-DM14 (new-day :-DM 0)))]
    (calc-di-and-dx (reduce sum-tr-and-dm initial-val adxs))))


(defn calc-single-adx [curr prev]
  (let [prev-adx (prev :ADX 0)]
    (if (> prev-adx 0)
      (assoc curr :ADX (dp 2 (div (+ (curr :DX) (* prev-adx 13) ) 14)))
      curr)))

(defn calc-tr14-full [adxs new-day]
  "To be used to calculate new values once TR14 has been initialised"
  (let [prev (last adxs)
        current (calc-tr-and-dm prev new-day)
        prev-tr-14 (prev :TR14)
        prev-plus-dm14 (prev :+DM14)
        prev-minus-dm14 (prev :-DM14)]
    (-> current
        (assoc :TR14 (+ (- prev-tr-14 (div prev-tr-14 14)) (current :TR)))
        (assoc :+DM14 (+ (- prev-plus-dm14 (div prev-plus-dm14 14)) (current :+DM)))
        (assoc :-DM14 (+ (- prev-minus-dm14 (div prev-minus-dm14 14)) (current :-DM)))
        calc-di-and-dx
        (calc-single-adx prev)
        )))

(defn set-initial-adx [adxs current]
  (assoc current :ADX (dp 2 (div
                              (+ (:DX current 0)
                                 (reduce #(+ %1 (%2 :DX 0)) 0 adxs))
                              14))))

(defn calc-adx [adxs new-day]
  (let [so-far (count adxs)
        values-required 14]
    (cond
     (= so-far (- (* 2 values-required) 1))
      (conj adxs (set-initial-adx adxs (calc-tr14-full adxs new-day)))
     (> so-far values-required)
      (conj adxs (calc-single-adx (calc-tr14-full adxs new-day) (last adxs)))
     (= so-far values-required)
      (conj adxs (calc-tr14-initial adxs                                                              (calc-tr-and-dm (last adxs) new-day)))
     (= so-far 0) (conj adxs new-day)
     (= 1 1) (conj adxs (calc-tr-and-dm (last adxs) new-day)))))

