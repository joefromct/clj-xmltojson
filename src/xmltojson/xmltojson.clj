(ns xmltojson.xmltojson
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str :refer [trim]]))

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
  "If XML is only a text node return that, to be stuffed into the parent tag.."
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
  {:pre [(every? map? [m1 m2])]}
  (let [merger (fn [m1 m2]
                 (into (maybe-vector m1)
                       (maybe-vector m2)))]
    (merge-with merger m1 m2)))

(declare parse)

(defn xml-merge-parts
  "Merges :attrs and :content to :tag of xml parsed through clojure.(data).xml.
  Applies post-processing via a postwalk.
    TODO other post-processing fn's?"
  [{:keys [attrs content]}
   {:keys [attrs-prefix strip-whitespace?]
    :or   {attrs-prefix "@" strip-whitespace? true}
    :as opt-map}]
  (merge (prefix-keywords attrs-prefix attrs)
         (cond
           ;; nothing here, quick and easy
           (nil? content) nil
           (empty? content) content
           (string? content) (hash-map :#text
                                       (if strip-whitespace? (trim content) content))
           ;; TODO do i need every here? It might be slow.
           (every? map? content) (reduce merge-to-vector
                                         (map #(parse % opt-map) content))
           (every? string? content) (->>  (if strip-whitespace?
                                            (map trim content)
                                            content)
                                          (str/join "\n")
                                          (hash-map :#text))
           ;; NOTE this is the nasty scenario where there is maps inside of the text,
           ;; sort of like html... often with  with <br> or <p> tags embedded
           ;; in form content.
           ;;
           ;; We take it in two parts; bundle all strings into #text, and then
           ;; anything a map should be again walked with the main function.
           :else (apply merge  {:#text
                                (->>  (if-let [strs (filter string? content)]
                                        (if strip-whitespace?
                                          (map trim strs)
                                          strs))
                                      str/join)}
                        (->> content
                             (filter map?)
                             (map #(parse % opt-map)))))))

(defn parse
  "Receives clojure xml map (as provided by clojure(.data).xml) and returns 
   json-like hash map.

  `opt-map` Optional second map with keys [force-list, attr-prefix,
  strip-whitespace?] specify lists to be forced into vectors,  what to prefix
  xml attributes with, and to strip-whitespace."
  ([{:keys [tag] :as xml-map}
    {:keys [force-list]
     :or   {force-list #{}}
     :as opt-map}]
   {:pre [(and (keyword? tag)
               (map? xml-map)
               (set? force-list))]
    :post [(map? %)]}
   (let [v (-> xml-map
               (xml-merge-parts opt-map)
               nil-if-empty
               xml-only-text)]
     ;; if tag is in force list, maybe-vector the result of content.
     (if (force-list tag)
       (hash-map tag (maybe-vector v))
       (hash-map tag v))))
  ([xml-map] (parse  xml-map {})))
