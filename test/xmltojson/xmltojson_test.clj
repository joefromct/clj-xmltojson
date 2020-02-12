(ns xmltojson.xmltojson-test
  (:require [clojure.test :refer [deftest is testing]]
            [jsonista.core :as j]
            [xmltojson.t-utils :as tu]
            [xmltojson.xmltojson :as t]))

(deftest prefix-keywords
  (testing "prefix keywords simple test"
    (let [xml-map {:tag :model, :attrs {:color "blue"}, :content [" MoonDart "]}]
      (is (= (t/prefix-keywords "@"   {:a 1} ) {(keyword "@a") 1}) "put @ on the front of the :a keyword.")
      (is (= (t/prefix-keywords "at-" {:a 1} ) {:at-a 1})          "put \"at-\" on the front of the a: keyword.")
      (is (= (t/prefix-keywords "at-" xml-map)
             {:at-tag :model, :at-attrs {:color "blue"}, :at-content [" MoonDart "]}
             )"test a few prefixed keywords."))))

(deftest xml-only-text
  (testing "If XML is only a text node return that."
    (is (= (t/xml-only-text {:#text "some text"}) "some text"))
    (is (= (t/xml-only-text {:#text "some text" :other "stuff"}) {:#text "some text", :other "stuff"}))
    (is (= (t/xml-only-text {:no-text "no text?" :other "stuff"}) {:no-text "no text?", :other "stuff"}))))

(deftest nil-if-empty
  (testing "nil-if-empty"
    (is (= (t/nil-if-empty []) nil))
    (is (= (t/nil-if-empty nil ) nil))
    (is (= (t/nil-if-empty [1 nil 2]) [1 nil 2] ))))

(deftest maybe-vector
  (testing "maybe-vector"
    (is (= (t/maybe-vector {:a :1}) [{:a :1}]))
    (is (= (t/maybe-vector  1     ) [1]))
    (is (= (t/maybe-vector  [1]   ) [1]))))

(deftest merge-to-vector
  (testing "merge-to-vector"
    (is (= (t/merge-to-vector  {:a "a", :b "b"}  {:a "a1", :b "b1"})
           {:a ["a" "a1"], :b ["b" "b1"]})   "merge these.")
    (is (= (t/merge-to-vector  {:a :1, :b 2 :c 9} {:a 1, :b 2})
           {:a [:1 1], :b [2 2] :c 9})   "merge these.")
    (is (= (t/merge-to-vector   {:a :1 :b 2 :c 9} {:a 1 :b 2})
           {:a [:1 1], :b [2 2] :c 9}) "missing some")
    (is (= (t/merge-to-vector  {:a "string! " :b "foo" :c 9} {:a 3 :b 2})
           {:a ["string! " 3], :b ["foo" 2], :c 9}) "some other tpes test")))

(deftest parse-strip-whitespace
  (testing "planes strip whitespace"  
    (let [xml-string-planes-small "<plane color=\"blue   \">
                                        <make> whitespace->>         </make>
                                        <year>1833    </year>
                                   </plane>"
          clojure-xml-small (tu/xml->cljxml xml-string-planes-small)
          output-json (t/parse clojure-xml-small {:attrs-prefix "@"})
          py-output (-> xml-string-planes-small
                        tu/py-xmltodict->json
                        (j/read-value (j/object-mapper {:encode-key-fn name
                                                        :decode-key-fn keyword})))
          output-json-ws (t/parse clojure-xml-small {:attrs-prefix "@" :strip-whitespace? false})]
      (is (= py-output output-json) "Python output should match Clojure output")
      (is (= output-json    {:plane {(keyword "@color") "blue   ", :make "whitespace->>", :year "1833" }})  "trimmed content")
      (is (= output-json-ws {:plane {(keyword "@color") "blue   ", :make " whitespace->>         ", :year "1833    "}}) "Should not be trimmed."))))

(deftest parse
 (testing "more planes and force list"
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
     (is (= (t/parse c-xml {:force-list force-list :attrs-prefix "at-"}  )
            {:planes
             {:transactions
              [{:at-id "ID1",
                :transaction
                [{:at-id "ID2", :ScriptID "Script id 100", :User "hal"}]}]}})
         "force list from small xml."
         )
     (is (= (t/parse c-xml {:force-list #{} :attrs-prefix "at-"}  )
            {:planes
             {:transactions
              {:at-id "ID1",
               :transaction
               {:at-id "ID2", :ScriptID "Script id 100", :User "hal"}}}})
         "empty force list from small xml."))))

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
  (testing "coercion - multiple to singles/etc."
    (let [opts    {:attrs-prefix "@" :force-list #{} :strip-whitepace? true}
          plane-0 (get-in (t/parse more-planes opts) [:planes :plane 0 ])
          plane-1 (get-in (t/parse more-planes opts) [:planes :plane 1 ]) ]
      (is (vector?  (:action plane-0)) "Multiple nodes with same key should coerce to vector.")
      (is (map?     (:action plane-1)) "Single node should coerce to map.")
      (is (= "1977" (:year plane-0))  "Single value test; First plane's year is \"1977\"")
      (is (= (get-in plane-1 [:action :#text])   "text!") ":#text keyword should be present with value.")
      (is (vector? (:owner plane-0)) "Owner on plane-0 should coerce to vector.")
      (is (string? (:owner plane-1)) "Owner on plane-0 should remain string."))))

(deftest xml->json-attributes
  (testing "attributes tests"
    (let [my-json-at- (t/parse more-planes {:attrs-prefix "at-"} )
          my-json-!-  (t/parse more-planes {:attrs-prefix "!-" })
          my-json-b1  (t/parse more-planes {})
          my-json-b2  (t/parse more-planes)]
      (is (contains? (get-in my-json-at- [:planes :plane 1 :action ]) :at-weather          )  "Prefix should work with 'at-'.")
      (is (contains? (get-in my-json-!-  [:planes :plane 1 :action ]) (keyword "!-weather")) "Prefix should work with '!-'.")
      (is (contains? (get-in my-json-b1  [:planes :plane 1 :action ]) (keyword "@weather") ) "Prefix should default to '@' as prefix.")
      (is (contains? (get-in my-json-b2  [:planes :plane 1 :action ]) (keyword "@weather") ) "Prefix should work with no opts to return '@' as prefix."))))

(deftest test-music-force-list
  (testing "Another verbose test with forcing country as a list."
    (let [xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                    <CATALOG>
                    <CD>
                        <TITLE>Empire Burlesque</TITLE>
                        <ARTIST>Bob Dylan</ARTIST>
                        <COUNTRY>USA</COUNTRY>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>Columbia</COMPANY>
                        <PRICE>10.90</PRICE>
                        <YEAR>1985</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Hide your heart</TITLE>
                        <ARTIST>Bonnie Tyler</ARTIST>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>CBS Records</COMPANY>
                        <PRICE>9.90</PRICE>
                        <YEAR>1988</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Greatest Hits</TITLE>
                        <ARTIST>Dolly Parton</ARTIST>
                        <COUNTRY>USA</COUNTRY>
                        <COMPANY>RCA</COMPANY>
                        <PRICE>9.90</PRICE>
                        <YEAR>1982</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Still got the blues</TITLE>
                        <ARTIST>Gary Moore</ARTIST>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>Virgin records</COMPANY>
                        <PRICE>10.20</PRICE>
                        <YEAR>1990</YEAR>
                    </CD>
                    </CATALOG> "
          py-output  (-> xml
                         (tu/py-xmltodict->json #{:COUNTRY} )
                         (j/read-value (j/object-mapper {:encode-key-fn name
                                                         :decode-key-fn keyword}))) 
          clj-output (-> xml
                         tu/xml->cljxml
                         (#(t/parse % {:force-list #{:COUNTRY}}))) ]
      (is (= py-output clj-output) "Py doesn't match clj?")
      (is (= {:CATALOG
              {:CD
               [{:COMPANY "Columbia"       ,:ARTIST "Bob Dylan"    ,:PRICE "10.90" ,:YEAR "1985" ,:TITLE "Empire Burlesque"    ,:COUNTRY ["USA" "UK"]}
                {:COMPANY "CBS Records"    ,:ARTIST "Bonnie Tyler" ,:PRICE "9.90"  ,:YEAR "1988" ,:TITLE "Hide your heart"     ,:COUNTRY ["UK"]}
                {:COMPANY "RCA"            ,:ARTIST "Dolly Parton" ,:PRICE "9.90"  ,:YEAR "1982" ,:TITLE "Greatest Hits"       ,:COUNTRY ["USA"]}
                {:COMPANY "Virgin records" ,:ARTIST "Gary Moore"   ,:PRICE "10.20" ,:YEAR "1990" ,:TITLE "Still got the blues" ,:COUNTRY ["UK"]}]}}
             clj-output)))))

(deftest xml->json-force-list
  (testing "Force list tests with Action being bundled."
    (let [my-json (t/parse more-planes {:attrs-prefix "!"
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
           "Each action in the planes->plane must be sequential as we forced-list."))))

(deftest py&clj-test-music
  (testing  "Jython/CLJ comparison for cd-catalog from  https://www.w3schools.com/xml/cd_catalog.xml"
    (let [force-list #{:COUNTRY}
          cds-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                    <CATALOG>
                    <CD>
                        <TITLE>Empire Burlesque</TITLE>
                        <ARTIST>Bob Dylan</ARTIST>
                        <COUNTRY>USA</COUNTRY>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>Columbia</COMPANY>
                        <PRICE>10.90</PRICE>
                        <YEAR>1985</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Hide your heart</TITLE>
                        <ARTIST>Bonnie Tyler</ARTIST>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>CBS Records</COMPANY>
                        <PRICE>9.90</PRICE>
                        <YEAR>1988</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Greatest Hits</TITLE>
                        <ARTIST>Dolly Parton</ARTIST>
                        <COUNTRY>USA</COUNTRY>
                        <COMPANY>RCA</COMPANY>
                        <PRICE>9.90</PRICE>
                        <YEAR>1982</YEAR>
                    </CD>
                    <CD>
                        <TITLE>Still got the blues</TITLE>
                        <ARTIST>Gary Moore</ARTIST>
                        <COUNTRY>UK</COUNTRY>
                        <COMPANY>Virgin records</COMPANY>
                        <PRICE>10.20</PRICE>
                        <YEAR>1990</YEAR>
                    </CD>
                    </CATALOG> "
          py-output (-> cds-xml
                        (tu/py-xmltodict->json force-list)
                        (j/read-value (j/object-mapper {:encode-key-fn name
                                                        :decode-key-fn keyword})))
          clj-output (-> cds-xml
                         tu/xml->cljxml
                         (t/parse {:force-list force-list}))]
      (is (= py-output clj-output )))))

(comment 
  (deftest xml-spec-complex
    (testing "TODO  this fails....ß xml spec is complex, and should test pretty much everything... i hope."
      (let [xml (slurp  "https://gist.githubusercontent.com/joefromct/cd9324f1d8fdbc658236a7081626ee55/raw/74ef7f6c462906980f78ae1368cc0aed0bccf309/xml-spec.xml")
            py-output (-> xml
                          tu/py-xmltodict->json 
                          (j/read-value (j/object-mapper {:encode-key-fn name
                                                          :decode-key-fn keyword})))
            clj-output (-> xml tu/xml->cljxml t/parse)
            ]
        (is (= py-output clj-output))
        ))))
