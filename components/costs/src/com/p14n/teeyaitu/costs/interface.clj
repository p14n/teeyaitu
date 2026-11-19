(ns com.p14n.teeyaitu.costs.interface
  (:require [com.p14n.teeyaitu.costs.core :as core]))

(def defaults
  "{:spread-bps 10 :slippage-bps 5 :funding-fee 0.025 :borrow-rate 0.005}. Confirm the
  funding fee against IG's current rate; replace :spread-bps with measured IG spreads."
  core/defaults)

(def zero-costs core/zero-costs)

(defn trade-cost [traded-notional params] (core/trade-cost traded-notional params))

(defn funding-cost [notional nights sonia params] (core/funding-cost notional nights sonia params))

(defn nights [from to] (core/nights from to))

(defn sonia-series [rows] (core/sonia-series rows))

(defn sonia-lookup [series] (core/sonia-lookup series))

(defn constant-sonia
  "For tests only; real backtests must use the daily Bank of England series."
  [rate]
  (core/constant-sonia rate))

(defn parse-boe-csv [text] (core/parse-boe-csv text))
