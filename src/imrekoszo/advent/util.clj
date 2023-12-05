(ns imrekoszo.advent.util
  (:require
    [clojure.java.io :as io]))

(defn input-seq [resource-name]
  (->> resource-name
    (io/resource)
    (io/reader)
    (line-seq)))
