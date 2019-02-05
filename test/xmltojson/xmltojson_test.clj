(ns xmltojson.xmltojson-test
  (:require [cheshire.core :as json]
            [clojure.string :as s]
            [clojure.test :refer [deftest is]]
            [xmltojson.predicates :as p]
            [xmltojson.utils :as u]
            [xmltojson.xmltojson :as t]
            [clojure.xml :as xml]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as j]
            [xmltojson.test-utils :refer [file-bn file-ext]]
            [xmltojson.pg-types]
            [clojure.string :as str]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            )
  #_(:import [java.io File ]))

(def pp pprint)

(deftest prefix-keywords
  (let [xml-map {:tag :model, :attrs {:color "blue"}, :content [" MoonDart "]}]
    (is (= (t/prefix-keywords "@"   {:a 1} ) {(keyword "@a") 1}) "put @ on the front of the :a keyword.")
    (is (= (t/prefix-keywords "at-" {:a 1} ) {:at-a 1})          "put \"at-\" on the front of the a: keyword.")
    (is (= (t/prefix-keywords "at-" xml-map)
           {:at-tag :model, :at-attrs {:color "blue"}, :at-content [" MoonDart "]}
           ) "test a few prefixed keywords.")))

(deftest xml-only-text
  "If XML is only a text node return that."
  (is (= (t/xml-only-text {:#text "some text"}) "some text"))
  (is (= (t/xml-only-text {:#text "some text" :other "stuff"}){:#text "some text", :other "stuff"}))
  (is (= (t/xml-only-text {:no-text "no text?" :other "stuff"}){:no-text "no text?", :other "stuff"})))

(deftest nil-if-empty
  (is (= (t/nil-if-empty []) nil))
  (is (= (t/nil-if-empty nil ) nil))
  (is (= (t/nil-if-empty [1 nil 2]) [1 nil 2] )))

(deftest merge-to-vector
  (is (= (t/merge-to-vector  {:a :1, :b 2 :c 9} {:a :1, :b 2})
         {:a [:1 :1], :b [2 2] :c 9})   "merge these.")
  (is (= (t/merge-to-vector   {:a :1 :b 2 :c 9} {:a :1 :b 2})
         {:a [:1 :1], :b [2 2] :c 9}) "missing some")
  (is (= (t/merge-to-vector  {:a "string! " :b :blah :c 9} {:a 3 :b 2})
         {:a ["string! " 3], :b [:blah 2], :c 9}) "some other tpes test"))

(deftest maybe-vector
  (is (= (t/maybe-vector {:a 1})[{:a 1}]))
  (is (= (t/maybe-vector  1    ) [1]))
  (is (= (t/maybe-vector  [1]  )[1]))
  )

(deftest maybe-vector-if-fl
  (is = ((t/maybe-vector-if-fl #{}      {:a 1 :b 2 :c 3}) {:a 1, :b 2, :c 3}) )
  (is = ((t/maybe-vector-if-fl #{:a :b} {:a 1 :b 2 :c 3}) {:a [1], :b [2], :c 3}))
  (is = ((t/maybe-vector-if-fl #{:D :E} {:a 1 :b 2 :c 3}) {:a 1, :b 2, :c 3}) ))

(deftest xml->json-strip-whitespace
  (let[xml-string-planes-small "<plane color=\"blue   \">
    <make> whitespace->>         </make>
    <year>1833    </year>
  </plane>"
       clojure-xml-small (u/xml-parse-str xml-string-planes-small)
       output-json       (t/xml->json clojure-xml-small {:attrs-prefix "at-" })
       output-json-ws    (t/xml->json clojure-xml-small {:attrs-prefix "at-" :strip-whitespace? false})
       ]
    (is (= output-json    {:plane {:at-color "blue   ", :make "whitespace->>", :year "1833" }})  "trimmed content")
    (is (= output-json-ws {:plane {:at-color "blue   ", :make " whitespace->>         ", :year "1833    "}}) "Should not be trimmed.")))


;; its sort of tuff to test the mutual recursion fn's?
(deftest xml->json
  (let [c-xml {:tag :planes,
               :attrs nil,
               :content
               [{:tag :transactions,
                 :attrs {:id "ID1"},
                 :content
                 [{:tag :transaction,
                   :attrs {:id "ID2"},
                   :content
                   [{:tag :ScriptID, :attrs nil, :content ["Script id 100"]}
                    {:tag :User, :attrs nil, :content ["hal"]}]}]}]}
        force-list #{:transactions :transaction }]
    (is (= (t/xml->json c-xml {:force-list force-list :attrs-prefix "at-"}  )
           {:planes
            {:transactions
             [{:at-id "ID1",
               :transaction
               [{:at-id "ID2", :ScriptID "Script id 100", :User "hal"}]}]}})
        "force list from small xml."
        )
    (is (= (t/xml->json c-xml {:force-list #{} :attrs-prefix "at-"}  )
           {:planes
            {:transactions
             {:at-id "ID1",
              :transaction
              {:at-id "ID2", :ScriptID "Script id 100", :User "hal"}}}})
        "empty force list from small xml."
        )))

(def more-planes {:tag :planes,
                  :attrs nil,
                  :content
                  [{:tag :plane,
                    :attrs nil,
                    :content
                    [{:tag :year, :attrs nil, :content ["1977"]}
                     {:tag :make, :attrs nil, :content [" Cessna "]}
                     {:tag :model, :attrs {:color "blue"}, :content [" MoonDart "]}
                     {:tag :owner, :attrs nil, :content ["mitch"]}
                     {:tag :owner, :attrs nil, :content ["glen"]}
                     {:tag :owner, :attrs nil, :content ["paulie"]}
                     {:tag :action,
                      :attrs
                      {:weather "windy",
                       :type "landing",
                       :whitespace_attribute "whitespace->  "},
                      :content ["sample text - landed the plane and it was windy. "]}
                     {:tag :action,
                      :attrs {:weather "sunny", :type "landing"},
                      :content
                      ["sample text - landed the plane and it was sunny. \t"]}]}
                   {:tag :plane,
                    :attrs nil,
                    :content
                    [{:tag :year, :attrs nil, :content ["1933"]}
                     {:tag :make, :attrs nil, :content [" Cessna "]}
                     {:tag :model, :attrs {:color "blue"}, :content [" Skyhawk "]}
                     {:tag :owner, :attrs nil, :content ["jim"]}
                     {:tag :action,
                      :attrs
                      {:weather "windy", :type "takeoff"},
                      :content ["text!"]}]}
                   {:tag :plane,
                    :attrs nil,
                    :content
                    [{:tag :year, :attrs nil, :content ["1833"]}
                     {:tag :make, :attrs nil, :content [" whitespace->>         "]}
                     {:tag :model, :attrs {:color "blue"}, :content [" puppy "]}
                     {:tag :owner, :attrs nil, :content ["hal"]}
                     {:tag :action,
                      :attrs {:weather "blowy", :type "takeoff"},
                      :content ["more with whitespace->>    "]}]}]})

(deftest xml->json-coercion
  (let [opts    {:attrs-prefix "@" :force-list #{} :strip-whitepace? true}
        plane-0 (get-in (t/xml->json more-planes opts) [:planes :plane 0 ])
        plane-1 (get-in (t/xml->json more-planes opts) [:planes :plane 1 ]) ]
    (is (vector?  (:action plane-0)) "Multiple nodes with same key should coerce to vector.")
    (is (map?     (:action plane-1)) "Single node should coerce to map.")
    (is (= "1977" (:year plane-0))  "Single value test; First plane's year is \"1977\"")
    (is (= (get-in plane-1 [:action :#text])   "text!") ":#text keyword should be present with value.")
    (is (vector? (:owner plane-0)) "Owner on plane-0 should coerce to vector.")
    (is (string? (:owner plane-1)) "Owner on plane-0 should remain string.")))

(deftest xml->json-attributes
  (let [my-json-at- (t/xml->json more-planes {:attrs-prefix "at-"} )
        my-json-!-  (t/xml->json more-planes {:attrs-prefix "!-" })
        my-json-b1  (t/xml->json more-planes {})
        my-json-b2  (t/xml->json more-planes)]
    (is (contains? (get-in my-json-at- [:planes :plane 1 :action ]) :at-weather          )  "Prefix should work with 'at-'.")
    (is (contains? (get-in my-json-!-  [:planes :plane 1 :action ]) (keyword "!-weather")) "Prefix should work with '!-'.")
    (is (contains? (get-in my-json-b1  [:planes :plane 1 :action ]) (keyword "@weather") ) "Prefix should default to '@' as prefix.")
    (is (contains? (get-in my-json-b2  [:planes :plane 1 :action ]) (keyword "@weather") ) "Prefix should work with no opts to return '@' as prefix.")))

(deftest xml->json-force-list
  "Force list tests with Action being bundled."
  (let [my-json (t/xml->json more-planes {:attrs-prefix "!"
                                          :force-list #{:action }}) ]
    (is (= my-json
           {:planes
            {:plane
             [{:make "Cessna",
               :year "1977",
               :action
               [{:!weather "windy",
                 :!type "landing",
                 :!whitespace_attribute "whitespace->  ",
                 :#text "sample text - landed the plane and it was windy."}
                {:!weather "sunny",
                 :!type "landing",
                 :#text "sample text - landed the plane and it was sunny."}],
               :owner ["mitch" "glen" "paulie"],
               :model {:!color "blue", :#text "MoonDart"}}
              {:make "Cessna",
               :year "1933",
               :action [{:!weather "windy", :!type "takeoff", :#text "text!"}],
               :owner "jim",
               :model {:!color "blue", :#text "Skyhawk"}}
              {:make "whitespace->>",
               :year "1833",
               :action
               [{:!weather "blowy",
                 :!type "takeoff",
                 :#text "more with whitespace->>"}],
               :owner "hal",
               :model {:!color "blue", :#text "puppy"}}]}}
           ))
    (is  (every? sequential? (map :action (get-in my-json [:planes :plane ])))
         "Each action in the planes->plane must be sequential as we forced-list.")))

(comment
  (def pg-db {:dbtype "postgresql"
              :dbname "public"
              :host "localhost"})

  (def force-list (->> (j/query pg-db "select * from force_list" )
                       (map :force_list)
                       (map keyword)
                       set))


  (let [xml-fn (fn [x & opts]
                 (assoc
                  (->> x
                       :xml
                       u/xml-parse-str
                       (#(t/xml->json % {:force-list force-list}))
                                        ;json/generate-string
                       (assoc x :val )
                       (#(dissoc % :xml ))
                       )
                  :program "clj fl")
                 )
        planes (j/query pg-db "select * from xmltest "
                        { :row-fn xml-fn})
        ]
    #_(pp planes)
    (j/execute!   pg-db ["delete from jsontest_clj where program = 'clj fl'"] )
    (j/insert-multi! pg-db :jsontest_clj planes)
    (println "finished insert")
    (j/execute!   pg-db ["update jsontest_clj set md5_checksum = md5(val::text)::uuid where md5_checksum is null"] ))
)
