(defproject teeyaitu "0.1.0-SNAPSHOT"
  :description "Price movement historic analysis"
  :url "http://www.p14n.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.4.0"],
                 [incanter/incanter "1.4.1"]
                 [enlive "1.1.5"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 ]
  :main teeyaitu.main
  :java-source-paths ["java/src","java/test"]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                 :plugins [[lein-midje "3.0.0"]]}})
