(defproject xmltojson "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 ;; this stuff is really just used for test, i should make
                 ;; another profile.
                 [cheshire "5.8.1"]
                 [com.taoensso/timbre "4.10.0"]
                 ]
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[org.postgresql/postgresql "42.2.5"]
                                      [org.clojure/java.jdbc "0.7.8"]
                                      [com.taoensso/timbre "4.10.0"]
                                      [org.clojure/data.xml "0.0.8"]]
                       :plugins      []}
             })
