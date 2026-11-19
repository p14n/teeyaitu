(ns com.p14n.teeyaitu.yahoo-client.core
  "Unofficial Yahoo daily chart endpoint, for the one-off research backfill only."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hato.client :as http]
            [jsonista.core :as json]
            [tablecloth.api :as tc])
  (:import (java.time Instant LocalDate ZoneId)))

(def ^ZoneId london (ZoneId/of "Europe/London"))
(def chart-url "https://query1.finance.yahoo.com/v8/finance/chart/")

(defn yahoo-symbol
  "Canonical LSE ticker -> Yahoo symbol: BARC -> BARC.L, BT.A -> BT-A.L."
  [ticker]
  (str (str/replace ticker "." "-") ".L"))

(defn- epoch-second [^LocalDate d]
  (.toEpochSecond (.atStartOfDay d london)))

(defn- price-scale
  "London prices usually come back in pence (GBp); store pounds."
  [currency]
  (case currency
    ("GBp" "GBX") 0.01
    1.0))

(defn parse-chart
  "Parses a chart JSON body (already decoded with string keys) into raw price rows."
  [ticker body ^Instant fetched-at]
  (let [result (get-in body ["chart" "result" 0])
        error  (get-in body ["chart" "error"])]
    (when (or error (nil? result))
      (throw (ex-info "Yahoo returned no data" {:ticker ticker :error error})))
    (let [scale  (price-scale (get-in result ["meta" "currency"]))
          ts     (get result "timestamp")
          quote  (get-in result ["indicators" "quote" 0])
          closes (get quote "close")
          vols   (get quote "volume")
          adj    (get-in result ["indicators" "adjclose" 0 "adjclose"])]
      (->> (map-indexed
            (fn [i t]
              (let [c (nth closes i nil)
                    a (or (nth adj i nil) c)
                    v (nth vols i nil)]
                (when c
                  {:date       (.toLocalDate (.atZone (Instant/ofEpochSecond (long t)) london))
                   :ticker     ticker
                   :close      (* scale (double c))
                   :adj-close  (* scale (double a))
                   :volume     (when v (double v))
                   :source     :yahoo
                   :fetched-at fetched-at})))
            ts)
           (remove nil?)
           ;; Yahoo occasionally repeats the latest bar; keep the last row per date.
           (reduce (fn [m r] (assoc m (:date r) r)) (sorted-map))
           vals
           vec))))

(defonce ^:private last-call (atom 0))

(defn- throttle! [min-interval-ms]
  (locking last-call
    (let [wait (- (+ @last-call (long min-interval-ms)) (System/currentTimeMillis))]
      (when (pos? wait) (Thread/sleep (long wait)))
      (reset! last-call (System/currentTimeMillis)))))

(defn- cache-file [cache-dir ticker from to]
  (io/file cache-dir (str ticker "_" from "_" to ".json")))

(defn fetch-chart-json
  "Returns the decoded chart JSON. Throttled; cached under :cache-dir when given."
  [ticker ^LocalDate from ^LocalDate to {:keys [min-interval-ms cache-dir]
                                         :or   {min-interval-ms 1000}}]
  (let [f (when cache-dir (cache-file cache-dir ticker from to))]
    (if (and f (.exists ^java.io.File f))
      (json/read-value f)
      (do
        (throttle! min-interval-ms)
        (let [resp (http/get (str chart-url (yahoo-symbol ticker))
                             {:query-params       {"period1"              (str (epoch-second from))
                                                   "period2"              (str (epoch-second (.plusDays to 1)))
                                                   "interval"             "1d"
                                                   "events"               "div,splits"
                                                   "includeAdjustedClose" "true"}
                              :headers            {"User-Agent" "Mozilla/5.0 (teeyaitu research backfill)"}
                              :as                 :string
                              :throw-exceptions?  false})]
          (when-not (= 200 (:status resp))
            (throw (ex-info "Yahoo request failed"
                            {:ticker ticker :status (:status resp)
                             :body (some-> (:body resp) (subs 0 (min 300 (count (:body resp)))))})))
          (when f
            (io/make-parents f)
            (spit f (:body resp)))
          (json/read-value (:body resp)))))))

(defn fetch-daily
  [ticker {:keys [from to] :as opts}]
  (let [to (or to (LocalDate/now london))]
    (tc/dataset (parse-chart ticker (fetch-chart-json ticker from to opts) (Instant/now))
                {:dataset-name (str ticker "-yahoo")})))
