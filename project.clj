(defproject xmltojson "0.1.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:uberjar {}
             :dev     [:profiles/dev]
             :test    [:profiles/dev :profiles/test]
             :profiles/dev {:dependencies [[pjstadig/humane-test-output "0.10.0"]
                                           [org.clojure/data.xml "0.0.8"]
                                           [com.rpl/specter "1.1.3"]
                                           [com.taoensso/timbre "4.10.0"]
                                           [metosin/jsonista "0.2.5"]
                                           [seancorfield/next.jdbc "1.0.13"] ;; using this to fetch everything from postgres for big tests.
                                           [org.postgresql/postgresql "42.2.9"]]
                            :resource-paths ["dev-resources/jython-standalone.jar"]
                            :plugins []
                            :injections [(require 'pjstadig.humane-test-output)
                                         (pjstadig.humane-test-output/activate!)]}
             :profiles/test {}}
  :test-selectors {:wip (fn [m]
                          (or (clojure.string/includes? (str (:ns m)) "wip")
                              (clojure.string/includes? (str (:name m)) "wip")))})
