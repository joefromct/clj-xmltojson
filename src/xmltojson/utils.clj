(ns xmltojson.utils
  (:require [clojure.xml :as xml])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn xml-parse-str
  "Just because clojure.xml doesn't have a parse-str function. Takes string s in
  xml format and xml/parse."
  [^String s]
  {:pre [(string? s)]}
  (xml/parse
   (java.io.ByteArrayInputStream. (.getBytes s))))

(defn md5
  "Returns md5 hash of input string s.
  (md5 \"joe\")
  => 8ff32489f92f33416694be8fdc2d4c22 "
  [^String s]
  {:pre [(string? s)]}
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))
