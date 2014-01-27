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
         (catch Exception e (println (str "Couldn't calc trades on " stock " " (.toString e)))))
    ))

(defn faky-all [ftse-3m-vals]
  (map #(faky-one % ftse-3m-vals) (get-saved-data-stocks)))

