(ns com.p14n.teeyaitu.price-store.interface
  (:require [com.p14n.teeyaitu.price-store.core :as core]))

(def columns
  "Raw price table columns: :date :ticker :close :adj-close :volume :source :fetched-at."
  core/columns)

(defn write-prices!
  "Merges a dataset (or seq of row maps) into <root>/prices/source=*/year=*/part.parquet,
  keeping the latest :fetched-at per [source ticker date]."
  [root data]
  (core/write-prices! root data))

(defn read-prices
  "Reads the raw price table as a dataset. opts: :sources, :years, :tickers."
  ([root] (core/read-prices root))
  ([root opts] (core/read-prices root opts)))

(defn quality-issues
  "Vector of issues (pence/pounds unit jumps, non-positive closes); empty when clean."
  [data]
  (core/quality-issues data))

(defn assert-quality!
  "Throws when quality-issues is non-empty; otherwise returns data."
  [data]
  (core/assert-quality! data))
