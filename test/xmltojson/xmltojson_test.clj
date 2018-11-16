(ns xmltojson.xmltojson-test
  (:require [cheshire.core :as json]
            [clojure.string :as s]
            [clojure.test :refer [deftest is]]
            [xmltojson.predicates :as p]
            [xmltojson.utils :as u]
            [xmltojson.xmltojson :as t]))

(deftest prefix-keywords
  (is (= (t/prefix-keywords "@"   {:a 1} ) {(keyword "@a") 1}))
  (is (= (t/prefix-keywords "at-" {:a 1} ) {:at-a 1})))

(deftest merge-to-vector
  "TODO this should have a force-list test..."
  (is (= (t/merge-to-vector #{} {:a :1, :b 2 :c 9}
                                {:a :1, :b 2})
         {:a [:1 :1], :b [2 2] :c 9})
      )
  ;; test simple force-list
  (is (= (t/merge-to-vector #{:c} {:a :1 :b 2 :c 9} {:a :1 :b 2})
         {:a [:1 :1], :b [2 2] :c [9]}))
  ;; test double force-list
  (is (= (t/merge-to-vector #{:a :c} {:a :1 :b 2 :c 9} {:a :1 :b 2})
         {:a [:1 :1], :b [2 2] :c [9]}))
  ;; force list that is not there
  (is (= (t/merge-to-vector #{:e} {:a :1 :b 2 :c 9} {:a :1 :b 2})
         {:a [:1 :1], :b [2 2] :c 9}))
  )

(deftest xml-only-text
  "If XML is only a text node return that."
  (is (= (t/xml-only-text {:#text "some text"}) "some text"))
  (is (= (t/xml-only-text {:#text "some text" :other "stuff"}){:#text "some text", :other "stuff"}))
  (is (= (t/xml-only-text {:no-text "no text?" :other "stuff"}){:no-text "no text?", :other "stuff"})))

(deftest nil-if-empty
  (is (= (t/nil-if-empty []) nil))
  (is (= (t/nil-if-empty nil ) nil))
  (is (= (t/nil-if-empty [1 nil 2]) [1 nil 2] )))

(def xml-string-planes (slurp "test/resources/planes.xml"))

(def clojure-xml (u/xml-parse-str xml-string-planes))

(deftest xml->json-coercion
  (let [
        opts {:attrs-prefix "@" :force-list #{}}
        plane-0 (get-in (t/xml->json clojure-xml opts) [:planes :plane 0 ])
        plane-1 (get-in (t/xml->json clojure-xml opts) [:planes :plane 1 ]) ]
    (is (vector?  (:action plane-0)) "Multiple nodes with same key should coerce to vector.")
    (is (map?     (:action plane-1)) "Single node should coerce to map.")
    (is (= "1977" (:year plane-0))  "Single value test; First plane's year is \"1977\"")
    (is (= (get-in plane-1 [:action :#text])   "text!") ":#text keyword should be present with value.")
    (is (vector? (:owner plane-0)) "Owner on plane-0 should coerce to vector.")
    (is (string? (:owner plane-1)) "Owner on plane-0 should remain string.")))

(deftest xml->json-attributes
  (let [my-json-at- (t/xml->json clojure-xml {:attrs-prefix "at-"})
        my-json-!-  (t/xml->json clojure-xml {:attrs-prefix "!-" })
        my-json-b1  (t/xml->json clojure-xml {})
        my-json-b2  (t/xml->json clojure-xml)]
    (is (contains? (get-in my-json-at- [:planes :plane 1 :action ]) :at-weather          ))  "Prefix should work with 'at-'."
    (is (contains? (get-in my-json-!-  [:planes :plane 1 :action ]) (keyword "!-weather"))) "Prefix should work with '!-'."
    (is (contains? (get-in my-json-b1  [:planes :plane 1 :action ]) (keyword "@weather") )) "Prefix should work with empty opts to return '@' as prefix."
    (is (contains? (get-in my-json-b2  [:planes :plane 1 :action ]) (keyword "@weather") )) "Prefix should work with no opts to return '@' as prefix."))

(deftest xml->json-force-list
  "TODO"
  (let [force-list #{:action }
        my-json (t/xml->json clojure-xml {:attrs-prefix "!" :force-list force-list})]
    (is (= my-json
           {:planes
            {:plane
             [{:make " Cessna ",
               :year "1977",
               :action [{:!weather "windy", :!type "landing", :#text "sample text - landed the plane and it was windy. "}
                        {:!weather "sunny", :!type "landing", :#text "sample text - landed the plane and it was sunny. "}],
               :owner ["mitch" "glen" "paulie"],
               :model {:!color "blue", :#text " MoonDart "}}
              {:make " Cessna ",
               :year "1933",
               :action [{:!weather "windy", :!type "takeoff", :#text "text!"}],
               :owner "jim",
               :model {:!color "blue", :#text " Skyhawk "}}]}}
           ))
    (is  (every? sequential? (map :action (get-in my-json [:planes :plane ])))
         "Each action in the planes->plane must be sequential as we forced-list.")))

