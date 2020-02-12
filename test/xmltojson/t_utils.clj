(ns xmltojson.t-utils
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is]]
            [xmltojson.xmltojson :as t]
            [jsonista.core :as j]
            [clojure.xml :as xml]
            [clojure.data.xml :as dxml]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.data :refer [diff]]
            [clojure.string :as str]
            [taoensso.timbre :as timbre :refer [log trace debug info warn error fatal report
                                                logf tracef debugf infof warnf errorf fatalf reportf
                                                spy get-env]])
  (:import (org.python.core Py)))

(defn xml->cljxml
  "Just a wrapper around clojure.xml because parse only works on ByteArrays"
  [xml-string]
  {:pre [(string? xml-string)]}
  (-> xml-string
      .getBytes
      java.io.ByteArrayInputStream.
      xml/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jython interop hacks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.caffeinatedideas.com/2019/07/16/building-clojure-jython-interop.html
;; So one issue we need to deal with is the conversion of data between Clojure
;; and Jython types. Clojure leverages Java built in types whenever possible.
;; This allows us to use a set of helper static methods on the
;; org.python.core.Py class to convert Java types to their corresponding Jython
;; types. Dispatch on which static method to use is based on the Java class so
;; we can use the type function along with a multimethod to make a very
;; declarative set of rules for converting types.

(defmulti cast:clj->py type)
(defmethod cast:clj->py java.lang.Integer  [obj] (Py/newInteger obj))
(defmethod cast:clj->py java.lang.Long     [obj] (Py/newLong obj))
(defmethod cast:clj->py java.lang.Float    [obj] (Py/newFloat obj))
(defmethod cast:clj->py java.lang.Double   [obj] (Py/newFloat obj))
(defmethod cast:clj->py java.lang.String   [obj] (Py/newString obj))
(defmethod cast:clj->py :default           [obj]
  (println (type (obj))) obj)

(defmulti cast:py->clj(fn [obj](str (type obj))))
(defmethod cast:py->clj "class org.python.core.PyInteger"    [obj] (.asInt obj))
(defmethod cast:py->clj "class org.python.core.PyLong"       [obj] (.asLong obj))
(defmethod cast:py->clj "class org.python.core.PyFloat"      [obj] (.asDouble obj))
(defmethod cast:py->clj "class org.python.core.PyString"     [obj] (.asString obj))
(defmethod cast:py->clj "class org.python.core.PyUnicode"    [obj] (.asString obj))
(defmethod cast:py->clj "class org.python.core.PyBoolean"    [obj] (Py/py2boolean obj))
(defmethod cast:py->clj :default                             [obj] obj)

(defn py-xmltodict->json
  "Run's jython's xmltodict parse function, with optional force-list (fl, set)."
  ([xml] (py-xmltodict->json xml #{}))
  ([xml fl]
   {:pre ((string? xml)
          (or (set? fl)
              (nil? fl)))}
   (spit "tmp.xml" xml)
   (with-open [python (org.python.util.PythonInterpreter.)]
     (.exec python "from xmltodict import parse;")
     (.exec python "from json import dumps;")
     (.exec python "with open('tmp.xml') as f: xml=f.read()")
     (if-let [py-fl (->> fl
                         (map #(format "'%s'" (name %)))
                         (clojure.string/join ",")
                         not-empty)]
       (cast:py->clj  (.eval python (format "dumps(parse(xml, force_list=[%s]))" py-fl)))
       (cast:py->clj (.eval python (format "dumps(parse(xml))")))))))


 
