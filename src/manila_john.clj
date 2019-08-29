(ns manila-john 
  "A CouchDB library which implements only what clutch does not."
  (:use com.ashafa.clutch)
  (:require [manila-john.util :as util]
            [cemerick.url :as url]
            [com.ashafa.clutch [utils :as utils]]))

(def native-couch-fns #{"_count" "_stats" "_sums"})

(defmethod view-transformer :clojurescript
  [_]
  (view-transformer :cljs))

(util/defaliases
  all-dbs all-databases
  create-db create-database
  db-info database-info
  get-db get-database
  delete-db delete-database
  replicate-db replicate-database
  put-doc put-document
  get-doc get-document
  delete-doc delete-document
  copy-doc copy-document
  update-doc update-document
  save-design-doc save-design-document
  all-docs all-documents
  with-db* com.ashafa.clutch/with-db*)

(defmacro with-test-db
  "Creates a randomly named test db on localhost:5984, runs the body inside with-db, and
   finally deletes the db."
  [& body]
  `(let [db# (str (or (System/getenv "MJ_TEST_SERVER") "http://localhost:5984")
                  "/test-" (java.util.UUID/randomUUID))]
     (try
       (create-db db#)
       (with-db db#
         ~@body)
       (finally
         (delete-db db#)))))

(defn view-language-options
  "Return [language other-options]."
  [options]
  (if (keyword? options)
    [options nil]
    [(or (:language options) :javascript) (dissoc options :language)]))

(defn fixture-with-docs
  "Take a collection of docs and options to bulk-update, and return a clojure.test fixture fn that loads
   the docs and runs its test inside with-test-db."
  [docs & options]
  #(with-test-db
     (apply bulk-update docs options)
     (%)))

(defn wrap-db-op [f]
  (fn [& [maybe-db & rest :as args]]
    (let [db-var #'com.ashafa.clutch/*database*
          db @db-var]
      (if (and (thread-bound? db-var)
               (not (identical? maybe-db db)))
        (apply f db args)
        (apply f (utils/url maybe-db) rest)))))

(defn wrap-db-op*
  "Like wrap-db-op, but doesn't pass db to the wrapped fn. Instead it ensures that the db
   is bound to com.ashafa.clutch/*database*, so that the db does not need to be passed to
   most clutch and manila-john fns."
  [f]
  (wrap-db-op
   #(with-db %
      (apply f %&))))

(defn map-leaves
  [f m]
  (into {} (for [[k v] m]
             (if (map? v)
               [k (map-leaves f v)]
               [k (f v)]))))

(defmacro defdbop
  "Same as defn, but wraps the defined function in another that transparently
   allows for dynamic or explicit application of database configuration as well
   as implicit coercion of the first `db` argument to a URL instance."
  [name-sym & body]
  `(do
     (util/defn-wrap ~name-sym wrap-db-op ~@body)
     (alter-meta! (var ~name-sym) update-in [:doc] str
                  "\n\n  When used within the dynamic scope of `with-db`, the initial `db`"
                  "\n  argument is automatically provided.")))

(defmacro defdbop*
  "Like defdbop, but using wrap-db-op*."
  [name-sym & body]
  `(do
     (util/defn-wrap ~name-sym wrap-db-op* ~@body)
     (alter-meta! #'~name-sym update-in [:doc] str
                  "\n\n Wrapped in wrap-db-op*.")))

(defmacro dbop
  "Same as fn wrapped in wrap-db-op."
  [& body]
  `(wrap-db-op (fn ~@body)))

(defmacro dbop*
  "Same as fn wrapped in wrap-db-op*."
  [& body]
  `(wrap-db-op* (fn ~@body)))

(defn view-server-fns-fn
  "A fn version of clutch's view-server-fns macro."
  [options fns]
  (let [[language options] (view-language-options options)
        {:keys [compiler language]} (view-transformer language)
        compiler (compiler options)
        compiler #(or (native-couch-fns %) (compiler %))
        fns (map-leaves compiler fns)]
    [language fns]))

(defdbop get-or-save-view [db ddoc-name view-key view-language compiled-view-fns
                           & [query-params-map post-data-map :as args]]
  (try
    (apply get-view db ddoc-name view-key args)
    (catch Exception e
      (let [ddoc-id (str "_design/" (url/url-encode ddoc-name))
            ddoc (or (get-document db ddoc-id) {:_id ddoc-id})]
        (if (= compiled-view-fns (:views ddoc))
          (throw e)
          (do
            (->> (assoc ddoc :language (name view-language) :views compiled-view-fns)
                 (put-document db))
            (apply get-view db ddoc-name view-key args)))))))

(defdbop* bulk-get [keys & [query-params-map]]
  (-> (merge query-params-map {:include_docs true})
      (all-docs {:keys keys})
      (->> (map :doc ))))

(defmacro defviews [options view-compiler-options & views]
  "Usage: (defviews \"forum\" :javascript
           (by-uri
            \"function (doc) {
                              emit (doc.uri)}\"
                              \"_count\"))
          (by-uri db {:key \"/asd\"})"
  (let [options (if (string? options) {:ddoc-prefix options} options)
        view-map (fn [[sym mapper reducer]]
                   {(keyword (name sym))
                    (if reducer ; :reduce nil makes Cloudant choke
                      {:map mapper
                       :reduce reducer}
                      {:map mapper})})
        [view-language] (view-language-options view-compiler-options)
        view-fns (apply merge (map view-map views))
        [compiled-view-language compiled-view-fns] (view-server-fns-fn view-compiler-options view-fns)
        ddoc-name (or (:ddoc-name options)
                      (str (or (:ddoc-prefix options) "manila-john-auto-") (hash view-fns)))
        sym-meta {:ddoc-name ddoc-name
                  :view-language view-language
                  :view-fns view-fns
                  :compiled-view-language compiled-view-language
                  :compiled-view-fns compiled-view-fns}
        clj-fn (fn [[sym]]
                 (let [sym-meta (assoc sym-meta :view-name (name sym))
                       sym (with-meta sym {:manila-john sym-meta})]
                   `(defdbop* ~sym [& args#]
                      (apply get-or-save-view ~ddoc-name ~(keyword (name sym))
                             ~compiled-view-language ~compiled-view-fns args#))))]
    `(do ~@(map clj-fn views))))

(defn add-missing-view-keys
  "Assoc keys as :startkey and :endkey depending on (:descending options) if they are not
   already present."
  [low-key high-key options]
  (let [[a b] (if (:descending options) [:endkey :startkey] [:startkey :endkey])]
    (merge {a low-key, b high-key} options)))

(defmacro emit-results [xs]
  ``(~'loop [xs# ~~xs]
      (~'when (~'seq xs#)
        (~'when (~'seq xs#)
          (~'let [[k# x#] (~'first xs#)
                  k# (~'clj->js k#)
                  x# (~'clj->js x#)]
            (~'js/emit k# x#)
            (~'recur (~'rest xs#)))))))

(defmacro emitter [bindings & body]
  (let [doc `doc#]
    ``(~'fn [~~doc]
        ~~(emit-results
           ``(~'let [~~doc (~'js->clj ~~doc {:keywordize-keys true})
                     ~~bindings [~~doc]]
               ~@body)))))
