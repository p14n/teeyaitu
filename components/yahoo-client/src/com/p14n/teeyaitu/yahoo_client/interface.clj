(ns com.p14n.teeyaitu.yahoo-client.interface
  (:require [com.p14n.teeyaitu.yahoo-client.core :as core]))

(defn fetch-daily
  "Daily history for a canonical LSE ticker (e.g. \"BARC\") as a dataset in the raw price
  table schema, prices in pounds. opts: :from, :to (LocalDate), :min-interval-ms, :cache-dir."
  [ticker opts]
  (core/fetch-daily ticker opts))

(defn parse-chart
  "Pure parser for a decoded chart JSON body; returns a vector of raw price rows."
  [ticker body fetched-at]
  (core/parse-chart ticker body fetched-at))

(defn yahoo-symbol [ticker] (core/yahoo-symbol ticker))
