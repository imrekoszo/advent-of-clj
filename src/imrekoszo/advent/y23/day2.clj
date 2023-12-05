(ns imrekoszo.advent.y23.day2
  (:require
    [clojure.string :as str]
    [imrekoszo.advent.util :as util]
    [net.cgrand.xforms :as x]))

(defn parse-game-id [game-header]
  (-> game-header
    (str/split #"\s")
    second
    parse-long))

(defn parse-grab [grab-description]
  (x/into {}
    (comp
      (map str/triml)
      (map #(str/split % #"\s"))
      (x/for [[cnt color] _]
        [color (parse-long cnt)]))
    (str/split grab-description #",")))

(defn parse-max-grabs [game-description]
  (transduce
    (map parse-grab)
    (completing #(merge-with max %1 %2))
    {}
    (str/split game-description #"\;")))

(defn possible-grab? [grab]
  (->> grab
    (merge-with -
      {"red" 12, "green" 13, "blue" 14})
    (vals)
    (every? nat-int?)))

(defn part1 [input]
  (transduce
    (comp
      (map #(str/split % #"\:"))
      (x/for [[header description] _
              :when (possible-grab? (parse-max-grabs description))]
        (parse-game-id header)))
    + 0
    input))

(defn part2 [input]
  (transduce
    (comp
      (map #(str/split % #"\:"))
      (map second)
      (map #(->> % parse-max-grabs vals (reduce *))))
    + 0
    input))

(comment

  (part1 (util/input-seq "data/y23/day2/test.txt")) ;; 8
  (part1 (util/input-seq "data/y23/day2/input.txt")) ;; 2268
  (part2 (util/input-seq "data/y23/day2/test.txt")) ;; 2286
  (part2 (util/input-seq "data/y23/day2/input.txt")) ;; 63542

  )
