(ns xmltojson.predicates)

(def not-nil? (complement nil?))

(defn one?[thing]
  "Returns true if we have one thing."
  (or (= 1 (count thing))))

(defn content-is-single? [{:keys [content] :as m}]
  (or (nil? content)
      (and (sequential? content)
           (one? content))))

(defn content-is-single-map?[{:keys [content] :as m} ]
  (and (content-is-single? m)
       (map? (first content))))

(defn content-is-list-of-maps?[{:keys [content] :as m}]
  (and (sequential? content)
       (not (content-is-single-map? content))
       (every? map? content)))

(defn content-map?
  "Check if a node content is a map i.e. has child nodes"
  [content]
  (map? (first content)))

;; TODO this doesn't work
(comment 
  (defn map-is-list-of-maps-with-one-key?[{:keys [content] :as m}]
    (and (map-is-list-of-maps? content)
         (every? one? (set (map keys content)) ))))


(defn dup-keys? [content]
  (when content
    (let [dkeys (count (->> content
                            (map :tag)
                            distinct
                            (filter identity)
                            ))
          n     (count content)]
      (not= dkeys n))))

