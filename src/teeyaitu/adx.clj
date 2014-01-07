(ns teeyaitu.adx)

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

(defn calc-tr [curr-hi curr-lo prev-close]
  (let [hi-less-lo (- curr-hi curr-lo)
        hi-less-close (- curr-hi prev-close)
        lo-less-close (- curr-lo prev-close)]
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

(defn calc-adx-initial [adxs new-day]
  "To be used to calculate the initial value of TR14"
  )
(defn calc-tr-and-dm [prev current]
  (-> current
      (assoc :TR (calc-tr (current :high) (current :low) (prev :close)))
      (assoc :+DM (calc-plus-dm (prev :high) (prev :low) (current :high) (current :low)))
      (assoc :-DM (calc-minus-dm (prev :high) (prev :low) (current :high) (current :low))))
  
(defn calc-adx-full [adxs new-day]
  "To be used to calculate new values once TR14 has been initialised"
  )

(defn calc-adx [adxs new-day]
  (let [so-far (count adxs)
        values-required 14]
      (cond (> so-far values-required) (calc-adx-full adxs new-day)
            (= so-far values-required) (calc-adx-initial adxs new-day)
            (= so-far 0) (conj adxs new-day)
            (conj adxs (calc-tr-and-dm (last adxs) new-day)))))

