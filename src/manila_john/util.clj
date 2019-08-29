(ns manila-john.util)

(defmacro defn-wrap
  "Like defn, but applies wrap-fn."
  [name-sym wrap-fn & body]
  `(do
     (defn ~name-sym ~@body)
     (alter-var-root #'~name-sym ~wrap-fn)))

(defmacro defalias [new-name old-name]
  `(let [doc-str# (:doc (meta #'~old-name))]
     (def ^{:doc doc-str#} ~new-name @#'~old-name)))

(defmacro def-do-multi [name-sym op & [args-xform]]
  `(let [form->goal# (partial list* '~op)
         args-xform# (or ~args-xform identity)]
     (defmacro ~name-sym [& forms#]
       (->> (args-xform# forms#)
            (map form->goal#)
            (list* `do)))))

(defmacro defaliases [& syms]
  `(do
    ~@(map
       (fn [[new-name old-name]]
         `(defalias ~new-name ~old-name))
       (partition 2 syms))))

#_(def-do-multi defaliases defalias (partial partition 2))

(defn padded-hex [num]
  (let [s (Integer/toHexString num)]
    (if (even? (count s))
      s
      (str "0" s))))

(defn base16-encode [x]
  {:pre [(not (number? x))]}
  (->> (byte-array x)
       (map padded-hex)
       (apply str)))

(defn md5 [x]
  {:pre [(not (number? x))]}
  (-> (java.security.MessageDigest/getInstance "MD5")
      (.digest (.getBytes x))))

(def base16-md5 (comp base16-encode md5))
