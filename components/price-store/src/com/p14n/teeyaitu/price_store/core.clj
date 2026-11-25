(ns com.p14n.teeyaitu.price-store.core
  "Raw daily price table stored as Parquet, partitioned like the S3 layout:
  <root>/prices/source=<source>/year=<yyyy>/part.parquet. Phase 1 writes to a local
  root; an S3 root comes with the production path."
  (:require [clojure.java.io :as io]
            [tablecloth.api :as tc]
            [tech.v3.libs.parquet :as parquet])
  (:import (java.time Instant LocalDate)))

(def columns [:date :ticker :close :adj-close :volume :source :fetched-at])

(defn ->rows [data]
  (if (tc/dataset? data) (tc/rows data :as-maps) data))

(defn partition-path [root source year]
  (str root "/prices/source=" (name source) "/year=" year "/part.parquet"))

;; --- storage normalisation -------------------------------------------------------

(defn- ->local-date [x]
  (cond (nil? x) nil
        (instance? LocalDate x) x
        (string? x) (LocalDate/parse x)
        (integer? x) (LocalDate/ofEpochDay (long x))
        :else (LocalDate/parse (str x))))

(defn- nan->nil [x]
  (when (and x (not (and (double? x) (Double/isNaN x)))) (double x)))

(defn- to-storage
  "Parquet-friendly values: strings for keywords and instants, NaN for missing volume."
  [r]
  {:date       (:date r)
   :ticker     (:ticker r)
   :close      (double (:close r))
   :adj-close  (double (or (:adj-close r) (:close r)))
   :volume     (double (or (:volume r) Double/NaN))
   :source     (name (:source r))
   :fetched-at (str (or (:fetched-at r) (Instant/now)))})

(defn- from-storage [r]
  (let [r (update-keys r keyword)]
    {:date       (->local-date (:date r))
     :ticker     (str (:ticker r))
     :close      (double (:close r))
     :adj-close  (double (:adj-close r))
     :volume     (nan->nil (:volume r))
     :source     (keyword (str (:source r)))
     :fetched-at (some-> (:fetched-at r) str Instant/parse)}))

(defn dedupe-rows
  "One row per [:source :ticker :date], keeping the latest :fetched-at."
  [rows]
  (->> rows
       (group-by (juxt :source :ticker :date))
       vals
       (map (fn [rs] (last (sort-by #(or (:fetched-at %) Instant/EPOCH) rs))))
       (sort-by (juxt :ticker :date))
       vec))

;; --- read / write ----------------------------------------------------------------

(defn read-partition [path]
  (if (.exists (io/file path))
    (mapv from-storage (tc/rows (parquet/parquet->ds path) :as-maps))
    []))

(defn- partition-files [root]
  (let [dir (io/file root "prices")]
    (when (.exists dir)
      (for [^java.io.File f (file-seq dir)
            :when (= "part.parquet" (.getName f))
            :let [year-dir (.getParentFile f)
                  src-dir  (.getParentFile year-dir)]]
        {:path   (.getPath f)
         :year   (Long/parseLong (subs (.getName year-dir) (count "year=")))
         :source (keyword (subs (.getName src-dir) (count "source=")))}))))

(defn write-prices!
  "Merges rows into their source/year partitions, de-duplicating. Returns written paths."
  [root data]
  (let [rows   (map #(update % :fetched-at (fn [t] (or t (Instant/now)))) (->rows data))
        groups (group-by (juxt :source #(.getYear ^LocalDate (:date %))) rows)]
    (vec
     (for [[[source year] new-rows] groups]
       (let [path   (partition-path root source year)
             merged (dedupe-rows (concat (read-partition path) new-rows))]
         (io/make-parents path)
         (parquet/ds->parquet (tc/dataset (map to-storage merged)) path)
         {:path path :source source :year year :rows (count merged)})))))

(defn read-prices
  "Reads the raw table. opts: :sources #{:yahoo :ig}, :years #{...}, :tickers #{...}."
  ([root] (read-prices root {}))
  ([root {:keys [sources years tickers]}]
   (let [files (cond->> (partition-files root)
                 sources (filter #(contains? (set sources) (:source %)))
                 years   (filter #(contains? (set years) (:year %))))
         rows  (cond->> (mapcat (comp read-partition :path) files)
                 tickers (filter #(contains? (set tickers) (:ticker %))))]
     (tc/dataset (sort-by (juxt :ticker :date :source) rows) {:dataset-name "prices"}))))

;; --- quality -----------------------------------------------------------------------

(defn- unit-jump? [ratio]
  (or (<= 95.0 ratio 105.0)
      (<= (/ 1.0 105.0) ratio (/ 1.0 95.0))))

(defn quality-issues
  "Flags one-day close moves of ~100x or ~1/100x per ticker and source: the signature of
  a series silently switching between pence and pounds. Also flags non-positive closes."
  [data]
  (->> (->rows data)
       (group-by (juxt :source :ticker))
       vals
       (mapcat
        (fn [rs]
          (let [rs (vec (sort-by :date rs))]
            (concat
             (for [r rs :when (not (pos? (double (:close r))))]
               {:ticker (:ticker r) :source (:source r) :date (:date r)
                :issue :non-positive-close :close (:close r)})
             (for [[a b] (partition 2 1 rs)
                   :when (and (pos? (double (:close a))) (pos? (double (:close b))))
                   :let [ratio (/ (double (:close b)) (double (:close a)))]
                   :when (unit-jump? ratio)]
               {:ticker (:ticker b) :source (:source b) :date (:date b)
                :issue :unit-jump :ratio ratio})))))
       (sort-by (juxt :ticker :date))
       vec))

(defn assert-quality!
  "Throws ex-info listing the issues if any; otherwise returns data unchanged."
  [data]
  (let [issues (quality-issues data)]
    (when (seq issues)
      (throw (ex-info (str (count issues) " price quality issue(s); fix before loading")
                      {:issues issues})))
    data))
