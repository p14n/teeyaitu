(ns com.p14n.teeyaitu.config.interface
  (:require [com.p14n.teeyaitu.config.core :as core]))

(defn load-config
  "Defaults from resources/config/defaults.edn, then env vars, then `overrides`."
  ([] (core/load-config))
  ([overrides] (core/load-config overrides)))

(defn env-overrides
  ([] (core/env-overrides))
  ([getenv] (core/env-overrides getenv)))

(defn deep-merge [& ms] (apply core/deep-merge ms))
