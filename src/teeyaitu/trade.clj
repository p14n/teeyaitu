(ns teeyaitu.trade
  (:use teeyaitu.data
        teeyaitu.core
                teeyaitu.adx
        clojure-csv.core))

(defn save-350-to-disk
  ([] (save-350-to-disk 350 (fetch-350-tickers)))
  ([last-x all-tickers]
   (map fetch-and-save-prices (take-last last-x all-tickers))))

(defn get-saved-data-stocks []
  (map
   #(let [name (.getName %)] (.substring name 0 (- (.length name) 4)))
   (seq (.listFiles (java.io.File. "data/")))))

(defn faky-one [stock ftse-3m-vals]
  (let [day-prices (csv-prices-to-day-map (load-values-from-csv stock))
        adx-values (reduce calc-adx [] day-prices)]
    (try (faky-trady stock adx-values ftse-3m-vals)
         (catch Exception e ( println (str "Couldn't calc trades on " stock " " (.toString e))) ))
    
    ))

(defn faky-all-to-csv [filename]
  (to-csv (sort-trades (flatten (faky-all {}))) filename))

(defn faky-all [ftse-3m-vals]
  (map #(faky-one % ftse-3m-vals) (get-saved-data-stocks)))

(defn look-for-setups [stock-name]
  (try
    (let [day-prices (csv-prices-to-day-map (take-last 250 (load-values-from-csv stock-name)))
          adx-values (reduce calc-adx [] day-prices)
          setups (find-setups (dec (count adx-values)) adx-values {} )]
      (if (:setup setups)
        (let [week (take-last 5 adx-values)
              today (last week)
              yesterday (nth week 3)
              long (bull-watch (:watch setups))
              open (if long (:high today) (:low today))]
          (-> setups
              (assoc :adx (last adx-values))
              (assoc :name stock-name)
              (assoc :week week)
              (assoc :long long)
              (assoc :stop (calc-initial-stop long week yesterday today))
              (assoc :open open)))
        {}))
    (catch Exception e
      (do (println (str "Couldn't calculate " stock-name " " (.getMessage e)))
          {}))))

(defn todays-setups []
  (filter #(not (empty? %)) (map look-for-setups (get-saved-data-stocks))))
