(ns imrekoszo.advent.util
  (:require
    [clojure.java.io :as io]))

(defn input-seq [resource-name]
  (->> resource-name
    (io/resource)
    (io/reader)
    (line-seq)))

(defn- first-rf
  ([] nil)
  ([x] x)
  ([_ x] (reduced x)))

(defn xfirst
  "Transform coll with xform and returns the first value or nil."
  [xform coll]
  (transduce xform first-rf coll))
