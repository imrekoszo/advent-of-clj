(ns imrekoszo.advent.y23.day1
  (:require
    [clojure.string :as str]
    [imrekoszo.advent.util :as util]
    [net.cgrand.xforms :as x]))

(def word->digit
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def digits
  (into #{} (map first) (vals word->digit)))

(def filter-digits-xf (filter digits))

(defn first-digit [s]
  (x/some filter-digits-xf s))

(defn last-digit [s]
  (x/some (comp filter-digits-xf x/last) s))

(defn encoded-number [s]
  (parse-long (str (first-digit s) (last-digit s))))

(defn part1 [input]
  (transduce (map encoded-number) + 0 input))

(let [re-digits
      (re-pattern (str/join "|" (into (vals word->digit) (keys word->digit))))]
  (defn first-digit' [s]
    (let [word-or-digit (re-find re-digits s)]
      (word->digit word-or-digit word-or-digit))))

(let [re-reverse-digits
      (re-pattern (str/join "|" (into (vals word->digit) (map str/reverse) (keys word->digit))))]
  (defn last-digit' [s]
    (let [word-or-digit
          (str/reverse (re-find re-reverse-digits (str/reverse s)))]
      (word->digit word-or-digit word-or-digit))))

(defn encoded-number' [s]
  (parse-long (str (first-digit' s) (last-digit' s))))

(defn part2 [input]
  (transduce (map encoded-number') + 0 input))

(comment

  (= 142 (part1 (util/input-seq "data/y23/day1/test1.txt")))
  (= 281 (part2 (util/input-seq "data/y23/day1/test2.txt")))

  (part1 (util/input-seq "data/y23/day1/input.txt")) ;; 54573
  (part2 (util/input-seq "data/y23/day1/input.txt")) ;; 54591
  )
