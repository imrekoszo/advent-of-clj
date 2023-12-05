(ns imrekoszo.advent.y23.day4
  (:require
    [clojure.math :as math]
    [clojure.set :as set]
    [imrekoszo.advent.util :as util]))

(defn part1 [input]
  (transduce
    (map #(->> %
            (re-seq #"(?<=\s)\d+(?=\s)|\d+$|\|")
            (partition-by #{"|"})
            ((juxt first last))
            (mapv (fn [s] (into #{} (map parse-long) s)))
            (apply set/intersection)
            (count)
            (dec)
            (math/pow 2)
            (long)))
    +
    input))

(comment

  (= 13 (part1 (util/input-seq "data/y23/day4/test.txt")))

  (part1 (util/input-seq "data/y23/day4/input.txt")) ;24848

  )
