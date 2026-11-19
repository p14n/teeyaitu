(ns com.p14n.teeyaitu.universe.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.time LocalDate)))

(defn- ->date [x]
  (cond (nil? x) nil
        (instance? LocalDate x) x
        (string? x) (LocalDate/parse x)
        :else (throw (ex-info "Not a date" {:value x}))))

(defn normalise
  "Parses :from/:to dates and indexes rows by ticker. :to is exclusive; nil means open."
  [membership]
  (let [members (mapv #(-> % (update :from ->date) (update :to ->date))
                      (:members membership))]
    (assoc membership
           :members members
           :by-ticker (group-by :ticker members))))

(defn load-membership
  ([] (load-membership "universe/ftse100.edn"))
  ([resource-path]
   (let [r (or (io/resource resource-path)
               (let [f (io/file resource-path)] (when (.exists f) f))
               (throw (ex-info "Membership file not found" {:path resource-path})))]
     (normalise (edn/read-string (slurp r))))))

(defn- covers? [{:keys [from to]} ^LocalDate date]
  (and (or (nil? from) (not (.isBefore date ^LocalDate from)))
       (or (nil? to) (.isBefore date ^LocalDate to))))

(defn member? [membership ticker date]
  (boolean (some #(covers? % date) (get-in membership [:by-ticker ticker]))))

(defn members-on [membership date]
  (->> (:members membership) (filter #(covers? % date)) (map :ticker) distinct sort vec))

(defn tickers [membership]
  (->> (:members membership) (map :ticker) distinct sort vec))

(defn survivorship-biased? [membership]
  (not (true? (:point-in-time? membership))))

(defn survivorship-warning [membership]
  (when (survivorship-biased? membership)
    (str "SURVIVORSHIP BIAS: membership '" (:index membership) "' as of " (:as-of membership)
         " is not point-in-time; results overstate returns and must not be trusted.")))

(defn load-epic-map
  ([] (load-epic-map "universe/ig-epics.edn"))
  ([resource-path] (edn/read-string (slurp (io/resource resource-path)))))
