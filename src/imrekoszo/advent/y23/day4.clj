(ns imrekoszo.advent.y23.day4
  (:require
    [clojure.math :as math]
    [clojure.set :as set]
    [imrekoszo.advent.util :as util]
    [net.cgrand.xforms :as x]))

(defn ->match-count [card-str]
  (->> card-str
    (re-seq #"(?<=\s)\d+(?=\s)|\d+$|\|")
    (partition-by #{"|"})
    ((juxt first last))
    (mapv set)
    (apply set/intersection)
    (count)))

;; nice trick:
;; https://github.com/tschady/advent-of-code/blob/88d39e43de692ebd34c317a107996382368c67cc/src/aoc/2023/d04.clj#L6C1-L7C62
;; (count (re-seq #"(?<=:.*)(?=\b(\d+)\b.*\|.*\b\1\b)" card-str))

(defn match-count->points [match-count]
  (long (math/pow 2 (dec match-count))))

(defn ->idx+match-count [idx card-str]
  [idx (->match-count card-str)])

(defn count-total-cards-rf
  ([] {:cards 0})
  ([acc] (:cards acc))
  ([acc [idx match-count]]
   (let [all-copies-of-current-card
         (inc (get acc idx 0))

         card-idxs-won-with-current-card
         (range (inc idx) (+ idx match-count 1))

         idx->count-won-with-current-card
         (zipmap card-idxs-won-with-current-card (repeat all-copies-of-current-card))]
     (->
       (merge-with + acc idx->count-won-with-current-card)
       (update :cards + all-copies-of-current-card)))))

(defn solve [input]
  (util/xfirst
    (x/transjuxt
      {:part1 (comp
                (map ->match-count)
                (map match-count->points)
                (x/reduce +))
       :part2 (comp
                (map-indexed ->idx+match-count)
                (x/reduce count-total-cards-rf))})
    input))

(comment
  (= {:part1 13 :part2 30} (solve (util/input-seq "data/y23/day4/test.txt")))

  (solve (util/input-seq "data/y23/day4/input.txt")) ;{:part1 24848, :part2 7258152}
  )
