(ns teeyaitu.report
  (:use teeyaitu.graph)
  (:require [net.cgrand.enlive-html :as html]))

;; (html/defsnippet link-model
;;   "/Users/ae589/dev/ws/clj/teeyaitu/reports/template.html"
;;   section-sel
;;   [{text :text href :href}] 
;;   [:a] (html/content text))



(def ^:dynamic *section-sel* {[:.title] [:.content]})

(defn str-content [x]
  (html/content (str x)))

(html/defsnippet price-row "template.html" [[:tr (html/nth-of-type 2)]]
  [day]
  [[:td (html/nth-of-type 1)]] (str-content (:date day))
  [[:td (html/nth-of-type 2)]] (str-content (:open day))
  [[:td (html/nth-of-type 3)]] (str-content (:high day))
  [[:td (html/nth-of-type 4)]] (str-content (:low day))
  [[:td (html/nth-of-type 5)]] (str-content (:close day))
  )

(html/defsnippet price-table "template.html" [:.prices]
  [week]
  [[:tr (html/nth-of-type 2)]] (html/content (price-row (nth week 0)))
  [[:tr (html/nth-of-type 3)]] (html/content (price-row (nth week 1)))
  [[:tr (html/nth-of-type 4)]] (html/content (price-row (nth week 2)))
  [[:tr (html/nth-of-type 5)]] (html/content (price-row (nth week 3)))
  [[:tr (html/nth-of-type 6)]] (html/content (price-row (nth week 4))))

(html/defsnippet chart-and-prices "template.html" [:.content]
  [model]
  [:img] (html/set-attr :src (str (:name model) ".png"))
  [:.prices] (html/content (price-table (:week model))))

(html/defsnippet section-model "template.html" *section-sel*
  [model]
  [:.title] (html/content (str (if (:long model) "Buy" "Sell") " "
                               (:name model) " at "
                               (:open model) ", stop "
                               (:stop model)))
  [:.content] (html/content (chart-and-prices model)))

(html/deftemplate report "template.html"
  [setups]
  [:.title] (html/content (str (count setups) " setups"))
  [:body] (html/content (map section-model setups)))


(defn save-report [setups name]
  (let [dir (str name "/")]
    (do (.mkDir (java.io.File. name)))
    (dorun (map #(png-of-last-3-months (:name %) dir) setups))
    (spit (str dir "report.html") (clojure.string/join (report setups)))))
