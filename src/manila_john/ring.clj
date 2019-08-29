(ns manila-john.ring
  (:require [manila-john :as mj]
            [com.ashafa.clutch :as clu]))

(defn wrap-with-db
  "Given a Ring handler and a fn that takes a Ring request and returns a
   db Url, use with-db to bind com.ashafa.clutch/*database* to the db so calls
   to clutch and manila-john need not specify it. Also assoc :couch-db to request."
  [f url-or-fn]
  (fn [request]
    (if-let [db (if (fn? url-or-fn) (url-or-fn request) url-or-fn)]
      (clu/with-db db
        (f (assoc request :couch-db db)))
      (f request))))
