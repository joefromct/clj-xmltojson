(defproject xmltojson "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ;; this stuff is really just used for test, i should make
                 ;; another profile.
                 [org.clojure/data.xml "0.0.8"]
                 [cheshire "5.8.1"]
                 [com.taoensso/timbre "4.10.0"]
                 ])
