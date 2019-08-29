(defproject manila-john "0.5.0-SNAPSHOT"
  :description "A Clojure library for Apache CouchDB."
  :url "https://github.com/john-shaffer/manila-john"
  :license {:name "BSD"
            :url "http://www.opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-1450" :optional true
                  :exclusions [com.google.code.findbugs/jsr305
                               com.googlecode.jarjar/jarjar
                               junit
                               org.apache.ant/ant
                               org.json/json
                               org.mozilla/rhino]]
                 [clj-http "0.5.5"]
                 [cheshire "4.0.0"]
                 [com.cemerick/url "0.1.0"]
                 [commons-codec "1.6"]
                 [slingshot "0.12.2"]]
  :min-lein-version "2.0.0"
  :repl-options {:init-ns manila-john}
  :test-selectors {:default #(not= 'test-docid-encoding (:name %))
                   :all (constantly true)})
