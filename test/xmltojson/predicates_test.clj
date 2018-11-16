(ns xmltojson.predicates-test
  (:require [clojure.test :as t :refer [deftest is]]
            [xmltojson.predicates :as sut]))

(def session   {:tag :session,
                :attrs {:fragment "0", :id "63873"},
                :content
                [{:tag :singleMap, :attrs {:map_attr "1"}, :content [{:tag :sm, :attrs nil, :content ["map!"]}]}
                 {:tag :properties,
                  :attrs
                  {:context "Agent",
                   :caption "Carrier CommercialUmbrella Pages US (9.1.0.5)",
                   :id "p1F74B",
                   :dateModified "2017-10-11"},
                  :content
                  [{:tag :userName, :attrs nil, :content ["ACEINA\\K4SWAN"]}
                   {:tag :houseColor, :attrs nil, :content ["green"]}
                   {:tag :houseNumber, :attrs nil, :content ["3913"]}
                   {:tag :notValue, :attrs nil, :content nil}
                   {:tag :limit,:attrs {:id "l5E8AE"},:content [{:tag :Type, :attrs nil, :content ["UmbrellaLimit "]} {:tag :sValue, :attrs nil, :content ["1000000"]}]}
                   {:tag :limit,:attrs {:id "lC1E45"},:content [{:tag :Type, :attrs nil, :content ["RetentionLimit"]} {:tag :iValue, :attrs nil, :content ["0      "]}]}]}]})

(def user-name  (get-in session [:content 1 :content 0 ] :content ))
(def nil-val    (get-in session [:content 1 :content 3 ] :content ))
(def single-map (get-in session [:content 0  ] :content           ))
(def properties (get-in session [:content 1  ] :content           ))

(deftest one?-test
  (let [one-vec  [:a]
        one-list '(:a)
        one-map  {:content :a}
        one-nil  nil
        two-vec  [:a :b]
        two-list '(:a :b)
        two-map  {:content :a :attrib [:blah 1 :foo 2 :bar 3 ]}
        ;; TODO not sure on this...
        ;;one-string  "a"
        ;;one-int      4
        ]
    (is (sut/one? one-vec    ))
    (is (sut/one? one-list   ))
    (is (not (sut/one? one-nil )))
    (is (not (sut/one? two-vec )))
    (is (not (sut/one? two-list)))
    (is (not (sut/one? two-map )))))

(deftest content-is-single?-test
  (is (sut/content-is-single? user-name       ))
  (is (sut/content-is-single? nil-val         ))
  (is (sut/content-is-single? single-map      ))
  (is (not (sut/content-is-single? session ) ))
  (is (not (sut/content-is-single? properties ))))

#_(deftest content-is-single-map?-test[{:keys [content] :as m} ]
  (and (content-is-single? m)
       (map? (first content))))

(deftest content-is-single-map?-test
  (is (not (sut/content-is-single-map? user-name  )))
  (is (not (sut/content-is-single-map? nil-val    )))
  (is      (sut/content-is-single-map? single-map ))
  (is (not (sut/content-is-single-map? session    )))
  (is (not (sut/content-is-single-map? properties ))))

(deftest content-is-list-of-maps?-test
  (is (not (sut/content-is-list-of-maps? user-name  )))
  (is (not (sut/content-is-list-of-maps? nil-val    )))
  (is      (sut/content-is-list-of-maps? single-map ))
  (is      (sut/content-is-list-of-maps? session    ))
  (is      (sut/content-is-list-of-maps? properties )))


