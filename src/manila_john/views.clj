(ns manila-john.views
  (:require [manila-john :as mj]))

(mj/defviews "manila-john.views" :javascript
  (conflicts-by-id
   "function (doc) {
                 if (doc._conflicts) {
                                      emit (doc._id, [doc._rev].concat (doc._conflicts))}}"
   "_count")
  (docs-by-type_subtype_created
   "function (doc) {
                   if (doc.type) {
                                  emit ([doc.type, doc.subtype, doc.created])}}"
   "_count"))

(mj/defdbop* doc-count
  ([] (:doc_count (mj/db-info)))
  ([type & [subtype start-date end-date options]]
     (-> {:startkey [type subtype start-date]
          :endkey [type subtype (or end-date {})]}
         (merge options)
         docs-by-type_subtype_created
         first :value (or 0))))

(defn doc-counter [type & [subtype]]
  (mj/dbop* [& [start-date end-date options :as args]]
            (apply doc-count type subtype args)))

(mj/defdbop* docs [& [type subtype start-date end-date options]]
  (->> {:startkey [type subtype start-date]
        :endkey [(or type {})  (or subtype {}) (or end-date {})]
        :reduce false
        :include_docs true}
       (merge options)
       docs-by-type_subtype_created
       (map :doc)))

(mj/defdbop* conflict-count [& [options]]
  (-> options
      conflicts-by-id
      first :value (or 0)))

(mj/defdbop* conflicts [& [options]]
  (->> (merge {:reduce false}
              options)
       conflicts-by-id
       (map :value)))
