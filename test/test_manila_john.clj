(ns test-manila-john
  (:use clojure.test
        manila-john)
  (:require [clojure.string :as str]))

(def products
  (map (partial zipmap [:title :price :uris])
       [["Gold" 20 ["/gold" "/oro"]]
         ["Wood" 5.5 ["/wood"]]]))

(use-fixtures :each
  (fixture-with-docs products))

(defviews "uris" :javascript
  (by-uri
   "function (doc) {
                   if (doc.uris) {
                                  for (var i in doc.uris) {
                                                           emit (doc.uris [i].toLowerCase (), [doc.title, doc.price])}}}"
   "_count"))

(defviews nil :cljs ;autocreate ddoc name
  (by-price
   (fn [doc]
     (when (.-price doc)
       (js/emit (.-price doc) (.-price doc))))
   "_stats"))

(defviews {:ddoc-prefix "titles-"} :javascript
  (by-title
   "function (doc) {
                   if (doc.title) emit (doc.title.toLowerCase ())}"))

(defdbop uri-count [db & [options]]
  (-> (by-uri db options) first :value (or 0)))

(defdbop* for-uri [uri & [options]]
  (-> {:key (str/lower-case uri)
       :limit 1
       :reduce false}
      (merge options)
      by-uri first))

(deftest test-js-views
  (is (nil? (for-uri "/211213")))
  (is (= "Gold" (-> "/oro" for-uri :value first)))
  (is (= 3 (uri-count)))
  (is (= "wood" (-> {:reduce false} by-title last :key))))

(deftest test-cljs-views
  (is (= 2 (:count (by-price))))
  (is (= 5.5 (:min (by-price))))
  (is (= "Wood" (->> {:include_docs true, :limit 1, :reduce false}
                     by-price first :doc :title))))

(deftest test-view-language-options
  (is (= [:javascript nil] (view-language-options :javascript)))
  (is (= [:javascript {:pretty-print true}] (view-language-options {:pretty-print true})))
  (is (= [:clojurescript {:pretty-print true}]
         (view-language-options {:language :clojurescript :pretty-print true}))))

(deftest test-defviews
  (let [uri-meta (-> #'by-uri meta :manila-john)
        price-meta (-> #'by-price meta :manila-john)]
    (testing "Test correct meta on view syms."
      (is (= "uris" (:ddoc-name uri-meta)))
      (is (= "by-uri" (:view-name uri-meta)))
      (is (= :javascript (:view-language uri-meta)))
      (is (= :javascript (:compiled-view-language uri-meta)))
      (is (= "function" (->> uri-meta :view-fns :by-uri :map (take 8))))
      (is (= "_count" (-> uri-meta :view-fns :by-uri :reduce)))
      (is (= (:view-fns uri-meta) (:compiled-view-fns uri-meta)))
      (is (= "manila-john-auto-" (take 17 (:ddoc-name price-meta))))
      (is (= "by-price" (:view-name price-meta)))
      (is (= :clojurescript (:view-language price-meta)))
      (is (= :javascript (:compiled-view-language price-meta)))
      (is (= '(fn [doc]) (->> price-meta :view-fns :by-price :map (take 2))))
      (is (= "_stats" (-> price-meta :view-fns :by-price :reduce)))
      (is (= "(function" (->> price-meta :compiled-view-fns :by-price :map (take 9))))
      (is (= "_stats" (-> price-meta :compiled-view-fns :by-price :reduce)))
      (is (= "titles-" (->> #'by-title meta :manila-john :ddoc-name (take 7)))))))

(deftest test-emitter
  (is (nil? (eval `(emit-results []))))
  (is (nil? (eval `(emitter [doc#] [[r#]]))))
  (is (= `fn (first (eval `(emitter [doc#] nil)))))
  (let [e (eval `(emitter [doc#]
                          (~`for [uri# (:uris doc#)]
                            [uri# (:title doc#)])))
        [_ e] (view-server-fns-fn :cljs {:e {:map e}})]
    (is (nil? e))
    (is (= [["/gold" "Gold"] ["/oro" "Gold"] ["/wood" "Wood"]]
           (->> (get-or-save-view "e" :e :javascript e)
                (map (juxt :key :value)))))))
