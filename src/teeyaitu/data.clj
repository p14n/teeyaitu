(ns teeyaitu.data
  (:use teeyaitu.core
        clojure-csv.core)
  (:require [net.cgrand.enlive-html :as html]))

(import java.util.Calendar)
;;components
;;http://uk.finance.yahoo.com/q/cp?s=%5EFTSE&c=0 1 2
;;http://uk.finance.yahoo.com/q/cp?s=%5EFTMC&c=0 1 2 3 4 5
(def ftse100root "https://uk.finance.yahoo.com/q/cp?s=%5EFTSE&c=")
(def ftse250root "https://uk.finance.yahoo.com/q/cp?s=%5EFTMC&c=")

(defn fetch-from-url[address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [buf (java.io.BufferedReader. 
                (java.io.InputStreamReader. stream))]
      (clojure.string/join "\n" (line-seq buf)))))

(defn create-cal [days-offset]
  (let [cal (Calendar/getInstance)]
    (do (.add cal Calendar/DATE days-offset))
    cal))


(defn construct-stock-url [name]
  (let [from-cal (create-cal -365)
        to-cal (create-cal 1)]
    (str "http://ichart.finance.yahoo.com/table.csv?s=" name
         "&d=" (.get to-cal Calendar/MONTH)
         "&e=" (.get to-cal Calendar/DATE)
         "&f=" (.get to-cal Calendar/YEAR)
         "&g="
         "&a=" (.get from-cal Calendar/MONTH)
         "&b=" (.get from-cal Calendar/DATE)
         "&c=" (.get from-cal Calendar/YEAR)
         "&ignore=.csv")))

(defn fetch-and-save-prices[stock]
  (try
    (let [file (str "data/" stock ".csv")]
      (do
        (println (str "Writing file " file))
        (spit file
              (fetch-from-url (construct-stock-url stock)))))
    (catch Exception e (println (str "Couldn't get " stock " " (.getMessage e))))))

(defn html-from-file[x]
  (html/html-resource (java.net.URL. (str "file:" x))))

(defn tickers-from-index-page [html]
  (map #(first (:content %))  (html/select html [:td.yfnc_tabledata1 :a])))

(defn tickers-from-url [url]
  (tickers-from-index-page (html/html-resource (java.net.URL. url))))

(defn fetch-350-tickers []
  (flatten (pmap tickers-from-url
                 [(str ftse100root "0")
                   (str ftse100root "1")
                   (str ftse100root "2")
                   (str ftse250root "0")
                   (str ftse250root "1")
                   (str ftse250root "2")
                   (str ftse250root "3")
                   (str ftse250root "4")
                   (str ftse250root "5")])))

(defn save-stock-prices [stocks]
  (pmap fetch-and-save-prices stocks))

(defn remove-blank-vals [prices]
  (filter #(not (= "000" (nth % 5))) prices))

(defn load-values-from-csv [stock]
  (remove-blank-vals (do (rest (parse-csv (clojure.string/replace
                                                 (slurp (str "data/" stock ".csv")) "-" ""))))))

(defn csv-prices-to-day-map [csv-prices] (map #(-> {:date (Integer/parseInt (first %))
                                          :high (to-bigdec (nth % 2))
                                          :low (to-bigdec (nth % 3))
                                          :close (to-bigdec (nth % 4))}) csv-prices))

(defn to-csv-row [t]
  (clojure.string/join "," (list (:date t) (:name t) (if (:long t) "Long" "Short") (:ATR t) (:open t) (:risk t) (:max-profit t) (:closedate t) (:profit t))))

(defn to-csv[trades filename]
  (let [rows (map to-csv-row trades)]
    (spit filename (str "Open date,Stock,Direction,ATR,Open,Risk,Max profit,Closed,Profit\n"
                        (clojure.string/join "\n" rows)))))

(defn sort-trades[trades] (sort #(- (:date %1 0) (:date %2 0)) trades))



