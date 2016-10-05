(ns teeyaitu.ig
  (:require [clj-http.client :as client]
            [clj-time.format :as f]
            [clj-time.core :as time]))

(def credentials (load-file "src/ig-credentials"))
(def ig-list (load-file "src/ig-list"))
(def default-env :demo)

;Request

(defn- generate-headers [context version]
  (assoc {} :CST (:CST context)
            :X-SECURITY-TOKEN (:XST context)
            :X-IG-API-KEY (:apikey context)
            :VERSION version))

(defn- build-url [href env]
  (str "https://" env "api.ig.com/gateway/deal" href))

(defn get-url [href context]
  (cond
    (= :test (:environment context)) (build-url href "net-")
    (= :uat (:environment context)) (build-url href "web-")
    (= :demo (:environment context)) (build-url href "demo-")
    (= :live (:environment context)) (build-url href "")
    :default (throw (Throwable. "Unknown environment"))))

(defn- build-response [response context]
  {:content (:body response)
   :context {:CST (:CST context)
             :XST (:XST context)}})

(defn get-generator [context href version]
  (let [response (client/get (get-url href context) {
                                                     :headers      (generate-headers context version)
                                                     :conn-timeout 10000
                                                     :content-type :json
                                                     :accept       :json
                                                     :as           :json
                                                     :insecure?    true})]
    (build-response response context)))

(defn get-with-query-params-generator [context href version query-param-map]
  (client/get (get-url href context) {
                                      :headers      (generate-headers context version)
                                      :query-params query-param-map
                                      :conn-timeout 10000
                                      :content-type :json
                                      :accept       :json
                                      :as           :json
                                      :insecure?    true}))

(defn post-generator [context href body version]
  (let [response (client/post (get-url href context) {
                                                      :body         body
                                                      :headers      (generate-headers context version)
                                                      :conn-timeout 10000
                                                      :content-type :json
                                                      :accept       :json
                                                      :as           :json
                                                      :insecure?    true})]
    (build-response response context)))


;Login

(defrecord LoginRequest [identifier password apikey environment])

(defn- generate-login-body [request]
  (client/json-encode (assoc {} :identifier (:identifier request) :password (:password request))))

(defn- generate-login-headers [request version]
  (assoc {} :X-IG-API-KEY (:apikey request) :VERSION version))

(defn- login-request [request version]
  (let [request-map {
                     :body         (generate-login-body request)
                     :headers      (generate-login-headers request version)
                     :conn-timeout 10000
                     :content-type :json
                     :accept       :json
                     :as           :json
                     :insecure?    true}]
    (println request-map)
    (client/post (get-url "/session" request) request-map)))

(defn login [request]
  (let [response (login-request request 2)]
    (if (= (:status response) 200)
      {:content (:body response)
       :context {:CST         (:CST (:headers response))
                 :XST         (:X-SECURITY-TOKEN (:headers response))
                 :environment (:environment request)
                 :apikey      (:apikey request)}
       }
      )))



;API
(defn authenticate [username password apikey environment]
  (login (LoginRequest. username password apikey environment)))

(def authenticateme
  (let [{username :user password :password key :key} (credentials default-env)]
    #(authenticate username password key default-env)))

(defn get-positions [context]
  (get-generator context "/positions" 2))

(defn get-working-orders [context]
  (get-generator context "/workingorders" 2))

(defn get-open-sprints [context]
  (get-generator context "/positions/sprintmarkets" 2))

(defn get-watchlists [context]
  (get-generator context "/watchlists" 1))

(defn get-accounts [context]
  (get-generator context "/accounts" 1))

(defn get-transactions [context type]
  (get-with-query-params-generator context "/history/transactions" 2 {"type" type }))

(defn get-applications [context]
  (get-generator context "/operations/application" 1))

(defn get-market [context epic]
  (get-generator context (str "/markets/" epic)  3))

(defn get-day-prices [context epic points]
  (get-generator context (str "/prices/" epic "/DAY/" points)  2))

(defn get-markets [context epics]
  (get-with-query-params-generator context "/markets"  2 {"epics" epics}))

(defn get-market-sentiment [context marketId]
  (get-generator context (str "/clientsentiment/" marketId) 1))

(defn find-epic [context search-term]
  (get-with-query-params-generator context "/markets" 1 {"searchTerm" search-term }))

(defrecord Otc-limit-order [direction epic size level expiry currencyCode forceOpen orderType])

(defn create-otc-order [context body]
  (post-generator context "/positions/otc" (client/json-encode body)  2))


(defn mid-price [price]
  (let [bid (:bid price)
        ask (:ask price)]
    (cond (nil? bid) ask
          (nil? ask) bid
          :else (/ (+ (:bid price) (:ask price)) 2))))

(defn format-prices [prices]
  (let [lines (map
                #(str
                  (.substring (clojure.string/replace (:snapshotTime %) "/" "") 0 8) ","
                  (mid-price (:openPrice %)) ","
                  (mid-price (:highPrice %)) ","
                  (mid-price (:lowPrice %)) ","
                  (mid-price (:closePrice %)) ",1,\n"
                  )
                prices)
        content (apply str lines)]
    content))

(def fmt (f/formatter "yyyyMMdd"))
(def igfmt (f/formatter "yyyy/MM/dd"))

(defn assure-file-exists [name]
  (let [filename (str "data/" name ".csv")]
    (if (not (.exists (clojure.java.io/as-file filename)))
      (spit filename "Date,Open,High,Low\n"))
    filename))

(defn last-date-in-file [filename]
  (let [lastline (last (.split (slurp filename) "\n"))]
    (if (= \2 (first lastline))
      (f/parse fmt (.substring lastline 0 8))
      nil)))

(defn days-since-last-recorded [lastdate]
  (if (nil? lastdate) 310 (time/in-days (time/interval lastdate (time/now)))))

(defn read-ig-epic [ctx name epic days]
  (do (println (str "Reading " name))
    {:name name :prices (get-in (get-day-prices ctx epic days) [:content :prices])}))

(defn read-ig-gold [ctx]
  (read-ig-epic ctx "GOLD" "MT.D.GC.MONTH1.IP" 1))

(defn filter-prices-by-date [date prices]
  (if (nil? date)
    prices
    (let [datestring (f/unparse igfmt date)
          filterf #(not (.startsWith (:snapshotTime %) datestring))]
      (drop 1 (drop-while filterf prices)))))

(defn read-and-save [ctx instrument]
  (let [name (:name instrument)
        filename (assure-file-exists name)
        lastdate (last-date-in-file filename)
        days (days-since-last-recorded lastdate)
        market (read-ig-epic ctx name (:epic instrument) days)]
    (spit filename (format-prices (filter-prices-by-date lastdate (:prices market))) :append true)
    market))

(defn save-result [{name :name prices :prices}]
  (let [filename (assure-file-exists name)
        lastdate (last-date-in-file filename)]
    (spit filename (format-prices (filter-prices-by-date lastdate prices)) :append true)
    prices))

(defn save-ig-to-disk []
  (let [{ctx :context} (authenticateme)]
    (map #(read-and-save ctx %) ig-list)))