(ns teeyaitu.core-test
  (:use teeyaitu.core
        midje.sweet))

(def val-mapper (create-mapper teeyaitu.sample.Value {:amt :amount}))
(def mappers {teeyaitu.sample.Value val-mapper})

(fact "I can use the mapper to create a new thrift object, set its name and retrieve the value"
  (let [new-object ((val-mapper :create-mapped))]
    (.setCurrency new-object "GBP")
    (.getCurrency new-object)) => "GBP")

(fact "My map of mappers has the mapper I'm expecting for the value class"
      (mappers teeyaitu.sample.Value) => val-mapper)


(fact "I can get the field from my mapper"
      (let [ccy-field ((val-mapper :find-field-by-name) "currency")]
        (.getFieldName ccy-field) => "currency"))

(fact "I get a null struct type for a String field"
      (let [ccy-field ((val-mapper :find-field-by-name) "currency")
            struct-finder (val-mapper :find-field-struct-type)]
        (struct-finder ccy-field) => nil))

(fact "I can map data to a new instance of Value"
      (let [new-object (thrift-from-map teeyaitu.sample.Value {:currency "GBP"} mappers)]
        (.getCurrency (first new-object)) => "GBP"))
