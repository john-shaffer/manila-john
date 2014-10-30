(ns manila-john.test-views
  (:use clojure.test
        manila-john.views)
  (:require [manila-john :as mj]))

(def users
  (->> [{:_id "a"} {:_id "b"} {:_id "cx"}]
       (map #(assoc % :type "manila-john/user"))))

(use-fixtures :each
  (mj/fixture-with-docs users))

(defn make-conflict [id]
  (-> id mj/get-doc (dissoc :_rev) (assoc :v 1)
      vector (mj/bulk-update :all_or_nothing true)))

(deftest test-conflict-count
  (is (= 0 (conflict-count)))
  (make-conflict "a")
  (is (= 1 (conflict-count))))

(deftest test-conflicts
  (is (empty? (conflicts)))
  (make-conflict "a")
  (is (= 1 (count (conflicts))))
  (is (= 2 (count (first (conflicts))))))

(deftest test-doc-count
  (is (= 3 (doc-count)))
  (is (= 3 (doc-count "manila-john/user")))
  (is (= 0 (doc-count "song"))))

(deftest test-doc-counter
  (let [user-count (doc-counter "manila-john/user")
        song-count (doc-counter "song")]
    (is (= 3 (user-count)))
    (is (= 0 (song-count)))))
