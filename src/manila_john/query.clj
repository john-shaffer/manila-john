(ns manila-john.query
  (:refer-clojure :exclude [filter key keys map take type val vals])
  (:require [clojure.core :as c]
            [com.ashafa.clutch :as cl]
            [manila-john :as mj]))

(declare type)

(defrecord Query [filter map reduce options db ddoc-name])

(defn query [& [x]]
  (cond
   (nil? x) (map->Query {})
   (string? x) (type x)
   (map? x) (map->Query x)
   :else (throw (IllegalArgumentException. "Can't convert to query."))))

(defn view-fn [q]
  (let [{:keys [filter map reduce]} (query q)]
    {:map (eval `(mj/emitter [~`doc]
                             (~`when (~`and ~@filter)
                               (~`concat ~@map))))
     :reduce reduce}))

(mj/defdbop* query-seq [q]
  (let [{:keys [db ddoc-name options]} (query q)
        compiled-fn (->> q view-fn (mj/view-server-fns-fn :cljs))
        view-name `view#
        compiled-fns {view-name compiled-fn}
        ddoc-name (or ddoc-name (name view-name))
        f #(mj/get-or-save-view ddoc-name view-name :javascript compiled-fns options)]
    (if db
      (cl/with-db db (f))
      (f))))

(defn filter [q & exprs]
  (update-in (query q) [:filter] into exprs))

(defn type [q & ts]
  (filter q (list (into (set) ts) '(:type doc))))

(defn map [q & exprs]
  (update-in (query q) [:map] into exprs))

(defn reduce [q expr]
  (assoc (query q) :reduce expr))

(defn assoc-options [q & opts]
  (update-in (query q) [:options] #(apply assoc % opts)))

(defn dissoc-options [q & opts]
  (update-in (query q) [:options] #(apply dissoc % opts)))

(defn ascending [& [q]]
  (dissoc-options q :descending))

(defn descending [& [q]]
  (assoc-options q :descending true))

(defn include-docs [& [q]]
  (assoc-options q :include_docs true))

(defn limit [q n]
  (assoc-options q :limit n))

(defn key [q k]
  (assoc-options q :key k))

(defn high-key [q k]
  (assoc-options q :high-key k))

(defn low-key [q k]
  (assoc-options q :low-key k))

(defn between [q k l]
  (if (< k l)
    (-> q (low-key k) (high-key l))
    (-> q (high-key k) (low-key l))))

(defn highest [q & exprs]
  (apply map (descending q) exprs))

(defn newest [& [q]]
  (highest q '[[(:created doc)]]))

(defn one [& [q]]
  (-> q (limit 1) query-seq first))

(defn doc [& [q]]
  (-> q include-docs one :doc))

(defn docs [& [q]]
  (->> q include-docs query-seq (c/map :doc)))

(defn id [& [q]]
  (-> q one :id))

(defn ids [& [q]]
  (->> q query-seq (c/map :id)))

(defn keys [& [q]]
  (->> q query-seq (c/map :key)))

(defn take [n q]
  (-> q (limit n) query-seq))

(defn val [& [q]]
  (-> q one :value))

(defn vals [& [q]]
  (->> q query-seq (c/map :value)))
