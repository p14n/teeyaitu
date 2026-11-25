(ns com.p14n.teeyaitu.config.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.p14n.teeyaitu.config.interface :as config]))

(deftest defaults-load
  (let [cfg (config/load-config)]
    (is (= :demo (get-in cfg [:ig :env])))
    (is (= 10 (get-in cfg [:costs :spread-bps])))
    (is (nil? (get-in cfg [:ig :password])) "no credentials in config")))

(deftest overrides-win
  (is (= 20 (get-in (config/load-config {:costs {:spread-bps 20}}) [:costs :spread-bps])))
  (is (= 5 (get-in (config/load-config {:costs {:spread-bps 20}}) [:costs :slippage-bps]))
      "deep merge keeps sibling keys"))

(deftest env-overrides
  (testing "only set variables override"
    (is (= {:ig {:env :live}}
           (config/env-overrides {"IG_ENV" "live" "TEEYAITU_BUCKET" ""})))))
