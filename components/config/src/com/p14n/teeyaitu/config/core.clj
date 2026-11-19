(ns com.p14n.teeyaitu.config.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn deep-merge
  "Recursively merges maps; later values win. Non-map values replace."
  [& ms]
  (apply merge-with
         (fn [a b] (if (and (map? a) (map? b)) (deep-merge a b) b))
         ms))

(defn read-resource [path]
  (if-let [r (io/resource path)]
    (edn/read-string (slurp r))
    (throw (ex-info "Config resource not found" {:path path}))))

(defn env-overrides
  "Builds an override map from environment variables. `getenv` is injectable for tests."
  ([] (env-overrides #(System/getenv %)))
  ([getenv]
   (let [v (fn [k] (let [s (getenv k)] (when-not (or (nil? s) (= "" s)) s)))]
     (cond-> {}
       (v "TEEYAITU_DATA_ROOT") (assoc-in [:data :root] (v "TEEYAITU_DATA_ROOT"))
       (v "TEEYAITU_BUCKET")    (assoc-in [:data :bucket] (v "TEEYAITU_BUCKET"))
       (v "IG_SECRET_ARN")      (assoc-in [:ig :secret-arn] (v "IG_SECRET_ARN"))
       (v "IG_ENV")             (assoc-in [:ig :env] (keyword (v "IG_ENV")))))))

(defn load-config
  ([] (load-config {}))
  ([overrides]
   (deep-merge (read-resource "config/defaults.edn")
               (env-overrides)
               overrides)))
