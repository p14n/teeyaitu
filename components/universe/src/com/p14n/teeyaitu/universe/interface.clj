(ns com.p14n.teeyaitu.universe.interface
  (:require [com.p14n.teeyaitu.universe.core :as core]))

(defn load-membership
  "Loads index membership rows [:ticker :from :to] (:to exclusive). Defaults to the
  bundled FTSE 100 file, which is today's constituents and therefore survivorship-biased."
  ([] (core/load-membership))
  ([resource-or-file-path] (core/load-membership resource-or-file-path)))

(defn normalise [membership] (core/normalise membership))

(defn member? [membership ticker date] (core/member? membership ticker date))

(defn members-on [membership date] (core/members-on membership date))

(defn tickers [membership] (core/tickers membership))

(defn survivorship-biased? [membership] (core/survivorship-biased? membership))

(defn survivorship-warning
  "A warning string for reports, or nil when membership is point-in-time."
  [membership]
  (core/survivorship-warning membership))

(defn load-epic-map
  ([] (core/load-epic-map))
  ([resource-path] (core/load-epic-map resource-path)))
