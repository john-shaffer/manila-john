(ns manila-john.test-query
  (:refer-clojure :exclude [])
  (:use clojure.test
        manila-john.query)
  (:require [clojure.core :as c]
            [manila-john :as mj]))

(def db-docs [{:_id "a" :type "post" :created "2016-08-13"}])

(use-fixtures :each
  (mj/fixture-with-dcs db-docs))

(deftest test-filter
  (let [q (map nil [[nil]])]
    (are [a b] (= a (ids (filter q b)))
         ["a" "v"] '(= "2016-08-13" (:created doc)))))

(deftest test-map
  (are [a b c] (= a (->> (map q b) (c/map c)))
       ["post" "post" "post" "user" "user"] '[[(:type doc)]] :key))
