(ns teeyaitu.main
  (:use teeyaitu.trade
        teeyaitu.report
        teeyaitu.ig))

(def sdf
  (java.text.SimpleDateFormat. "yyyyMMdd"))

(defn -main [& [arg]]
  (let [dayname (.format sdf (java.util.Date.))]
    (cond (= arg "data")
          (save-350-to-disk)
          (= arg "ig")
          (save-ig-to-disk)
          (= arg "backtest")
          (faky-all-to-csv (str dayname ".csv"))
          (= arg "setups")
          (save-report (todays-setups) dayname)
          (= 1 1)
          (println "data, backtest or setups"))))
