(ns xmltojson.xmltojson
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :refer [postwalk postwalk-demo]]))

(defn prefix-keywords
  "Prefixes keywords in a map, and returns same map with prefixed arguments.

    (prefix-keywords  \"new_\" )
    => {:new_a 1} "
  [attrs-prefix hash-map ]
  { :pre [(and (or (map? hash-map)
                   (nil? hash-map))
               (string? attrs-prefix))]
   :post [(map? %)]}
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

(defn maybe-vector
  "tucks things into a vector, if they aren't already."
  [v]
  {:post [(vector? %)]}
  (if (vector? v) v
      (vector v)))

(defn merge-to-vector
  "With two input maps, associate common keys into vectors and return result map
  containing all input data.  Coerces common key values into vectors.
  Input maps:
    {:a 1 :b 2 :c 3}
    {:a 2 :b 4 }
  Example Output
  {:a [1 2] :b [2  4] :c 3}
  "
  [m1 m2]
  {:pre [(every? map? [m1 m2])]
   ;; TODO
   ;;:post [(map? %)]
   }
  (->> (merge-with #(into (maybe-vector %1)
                          (maybe-vector %2))
                   m1
                   m2)))

(defn maybe-vector-if-fl
  "With a force-list set and a maybe-map coerce keys in force list into maybe-vectors."
  [fl m]
  {:pre [(set? fl)]}
  (if (map? m)
    (into m (for [[k v]
                  (select-keys m fl)]
              [k (maybe-vector v)]))
    m))

(defn my-postwalk[force-list m]
  "If we have a force-list on walk it after the final map is constructed to
  ensure we didn't miss any arrays/lists."
  {:pre [(and (set? force-list)
              (map? m))]}
  (if (not-empty force-list)
    (postwalk (partial maybe-vector-if-fl force-list) m)
    m))

(declare xml->json)

(defn xml-merge-parts
  "Merges :attrs and :content to :tag of xml parsed through clojure.(data).xml.
  Applies post-processing via a postwalk.
    TODO other post-processing fn's?"
  [{:keys [attrs content tag] :as xml-map}
   {:keys [force-list attrs-prefix strip-whitespace?]
    :or   {force-list #{} attrs-prefix "@" strip-whitespace? true}
    :as opt-map}]
  (my-postwalk force-list
            (merge (prefix-keywords attrs-prefix attrs)
                   (cond
                     ;; nothing here.
                     (nil? content) nil
                     ;; TODO what if we have a some maps but not all?
                     (map? (first content)) (reduce merge-to-vector
                                                    (map #(xml->json % opt-map) content))
                     ;; something here, but not a seq
                     :else (hash-map  :#text
                                      (if strip-whitespace? (str/trim (first content))
                                          (first content)))))))

(defn xml->json
  "Receives xml map (as provided by clojure.xml) and returns json-like hash map.

  `opt-map` Optional second map with keys [force-list, attr-prefix,
  strip-whitespace?] specify lists to be forced into vectors,  what to prefix
  xml attributes with, and to strip-whitespace .
  "
  ([{:keys [tag] :as xml-map}
    {:keys [force-list attrs-prefix strip-whitespace?]
     :or   {force-list #{} attrs-prefix "@" strip-whitespace? true}
     :as opt-map}]
   {:pre [(and (keyword? tag)
               (map? xml-map)
               (set? force-list)
               (string? attrs-prefix)
               (boolean? strip-whitespace?))]
    :post [(map? %)]}
   (hash-map tag (-> xml-map
                      (xml-merge-parts opt-map)
                      nil-if-empty
                      xml-only-text)))
  ([xml-map] (xml->json  xml-map {} )))
