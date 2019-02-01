(ns xmltojson.xmltojson
  (:require
   [clojure.spec.alpha :as s]))

(defn prefix-keywords
  "Prefixes keywords in a map, and returns same map with prefixed arguments.

    (prefix-keywords  \"new_\" )
    => {:new_a 1} "
  [attrs-prefix hash-map ]
  { :pre [(and (or (map? hash-map)
                   (nil? hash-map)))]}
  (let [prefix-keyword (fn[kw]  (->> kw
                                    name
                                    (format "%s%s" attrs-prefix)
                                    keyword))]
    (zipmap (map #(prefix-keyword % ) (keys hash-map ))
            (vals hash-map))))

(defn xml-only-text
  "If XML is only a text node return that.
  TODO make the text key configurable?"
  [m]
  (if (= (keys m) '(:#text))
    (val (first m))
    m))

(defn nil-if-empty
  "If empty return nil. Useful for a list like this: [] to go to this: nil "
  [m]
  (if (empty? m) nil m))

(defn merge-to-vector
  "With two input maps, associate common keys into vectors and return result map
  containing all input data.  Coerces common key values into vectors.
  Input maps:

    {:a 1 :b 2 :c 3}
    {:a 2 :b 4 }
  Example Output
  {:a [1 2] :b [2  4] :c 3}

  "
  [force-list m1 m2]
  {:pre [(and (map? m1)
              (map? m2)
              (set? force-list))]
   :post [(map? %)]}
  (let [to-vector (fn[x](if (vector? x) x
                           (vector x)))
        ;; find the forced-lists that are in our keys.
        fl's      (set (filter force-list (concat  (keys m1)
                                                   (keys m2))))
        ;; This takes a force-list value and returns a function that updates a
        ;; map and makes sure that forced-list is a vector.
        update-fl (fn[fl] (fn[x](update-in x [fl] to-vector)))
        ;; This composes a list of functions as created above, from fl values.
        update-fl's (apply comp (map update-fl fl's))
        ]
    (-> (merge-with #(into (to-vector %1)
                           (to-vector %2))
                    m1 m2)
        update-fl's)))

(defn xml-merge-parts
  "Merges :attrs and :content to :tag of xml parsed through clojure.(data).xml."
  [fn-prefix
   fn-walk-coll
   force-list
   {:keys [attrs content tag]}]
  (merge (fn-prefix attrs)
         (cond
           ;; nothing here.
           (nil? content) nil
           ;; TODO what if we have a some maps but not all?
           (map? (first content)) (reduce (partial merge-to-vector force-list )
                                          (map fn-walk-coll content))
           ;; something here, but not a seq
           :else (hash-map  :#text (first content)))))

(defn xml->json
  "Receives xml map (as provided by clojure.xml) and returns json-like hash map.

  `opt-map` Optional second map with keys [force-list, attr-prefix] specify
  lists to be forced into vectors and what to prefix xml attributes with.
  TODO more docs.
  "
  ([{:keys [tag] :as xml-map}
    opt-map]
   {:pre [(keyword? tag)]}
   (let [^set force-list   (or (:force-list   opt-map) #{})
         attrs-prefix      (or (:attrs-prefix opt-map) "@")
         strip-whitespace? (or (:strip-whitespace? opt-map) false)
         fn-prefix        (partial prefix-keywords attrs-prefix)
         fn-walk-coll     #(xml->json % opt-map)
         fn-merge-xml     (partial xml-merge-parts fn-prefix fn-walk-coll force-list)
         ]
     (hash-map tag (-> xml-map
                       fn-merge-xml
                       nil-if-empty
                       xml-only-text))))
  ([xml-map] (xml->json  xml-map {} )))
