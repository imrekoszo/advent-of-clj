(ns imrekoszo.advent2023.util
  (:require [clojure.java.io :as io]))

(defn input-seq [resource-name]
  (->> resource-name
    (io/resource)
    (io/reader)
    (line-seq)))

(defn parse-input-fn [parse]
  (fn [resource-name]
    (-> resource-name input-seq parse)))
