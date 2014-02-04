(ns teeyaitu.graph
  (:use teeyaitu.core
        teeyaitu.data))
   (require '[incanter.core :as i]
            '[incanter.charts :as c]
            'incanter.datasets)

(defn create-chart-from-dataset [dataset] (c/line-chart (i/sel dataset :cols :date) (i/sel dataset :cols :close)))

(defn create-chart [stock days]
  (let [v (load-values-from-csv stock)
        m (csv-prices-to-day-map v)
        day-values (take-last days m)
        dataset (i/to-dataset day-values)]
    (create-chart-from-dataset dataset)))

(defn png-of-last-3-months [stock dir]
  (i/save (create-chart stock 90) (str dir stock ".png")))
