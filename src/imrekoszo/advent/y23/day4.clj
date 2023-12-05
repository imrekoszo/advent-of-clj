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
    (mapv (fn [s] (into #{} (map parse-long) s)))
    (apply set/intersection)
    (count)))

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
  (x/some
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
