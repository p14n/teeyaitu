(ns teeyaitu.report
  (:use teeyaitu.graph)
  (:require [net.cgrand.enlive-html :as html]))

;; (html/defsnippet link-model
;;   "/Users/ae589/dev/ws/clj/teeyaitu/reports/template.html"
;;   section-sel
;;   [{text :text href :href}] 
;;   [:a] (html/content text))



(def ^:dynamic *section-sel* {[:.title] [:.content]})

(html/defsnippet price-row "template.html" [[:tr (html/nth-of-type 2)]]
  [day]
  [[:td (html/nth-of-type 1)]] (html/content (:date day))
  [[:td (html/nth-of-type 2)]] (html/content (:open day))
  [[:td (html/nth-of-type 3)]] (html/content (:high day))
  [[:td (html/nth-of-type 4)]] (html/content (:low day))
  [[:td (html/nth-of-type 5)]] (html/content (:close day)))

(html/defsnippet price-table "template.html" [:.prices]
  [week]
  [[:tr (html/nth-of-type 2)]] (price-row (nth week 0))
  [[:tr (html/nth-of-type 3)]] (price-row (nth week 1))
  [[:tr (html/nth-of-type 4)]] (price-row (nth week 2))
  [[:tr (html/nth-of-type 5)]] (price-row (nth week 3))
  [[:tr (html/nth-of-type 6)]] (price-row (nth week 4)))

(html/defsnippet chart-and-prices "template.html" [:.content]
  [model]
  [:img] (html/set-attr :src (str (:name model) ".png"))
  [:.prices] (price-table (:week model)))

(html/defsnippet section-model "template.html" *section-sel*
  [model]
  [:.title] (html/content (:name model))
  [:.content] (chart-and-prices model))


(html/deftemplate report "template.html"
  []
  [:.title] (html/content "Open a Barc"))
