(ns imrekoszo.advent.y23.day3
  (:require
    [imrekoszo.advent.util :as util]
    [net.cgrand.xforms :as x])
  (:import
    (java.util.regex Matcher)))

(set! *warn-on-reflection* true)

(defn match-coords
  "Returns the coordinates of the first character of the current match of matcher occupies.

  Coordinate format is [row-index character-position]"
  [row-idx ^Matcher matcher]
  [row-idx (.start matcher)])

(defn match-adjacent
  "Returns the set of coordinates surrounding the current match of matcher.

  Coordinate format is [row-index character-position]"
  [row-idx ^Matcher matcher]
  (let [s (.start matcher)
        e (.end matcher)
        c-range (set (range s e))
        c-around (range (dec s) (inc e))]
    (x/into #{}
      (x/for [c _ r (range (dec row-idx) (+ 2 row-idx))
              :when (not (and (= row-idx r) (c-range c)))]
        [r c])
      c-around)))

(defn token-data-fn
  "Returns a function to be used with map-indexed and which returns an
  iterable of token information found in a row.

  Tokens are either numbers or symbols matching sym-re.
  Return format is [:kw-of-type info]
  Info is a map of :value and :adjacent coords for numbers,
  and simply coords for symbols"
  [sym-re]
  (fn [row-idx row]
    (let [!matcher (re-matcher (re-pattern (str sym-re "|\\d+")) row)]
      (iteration
        (fn [_] (re-find !matcher))
        :vf #(if (re-find #"\d" %)
               [:number
                {:value (parse-long %)
                 :adjacent (match-adjacent row-idx !matcher)}]
               [:sym-coords
                (match-coords row-idx !matcher)])))))

(defn numbers-next-to-symbols
  "Looks at data from 3 adjacent rows and returns an eduction of the numbers
   from the middle row that are adjacent to a symbol on any row"
  [three-rows]
  (x/for [{:keys [value adjacent]} (:number (second three-rows))
          :when (x/some (keep #(some adjacent (:sym-coords %))) three-rows)]
    value))

(defn product-next-to-symbols-next-to-2-numbers
  "Looks at data from 3 adjacent rows and returns an eduction of the products
  of the numbers (on any of the rows) that are next to a symbol on the middle
  row, which is next to exactly 2 numbers"
  [three-rows]
  (x/for
    [sym-coord (:sym-coords (second three-rows))
     :let [number-values
           (x/into []
             (comp
               (mapcat :number)
               (filter some?)
               (filter #((:adjacent %) sym-coord))
               (map :value)
               (take 3))
             three-rows)]
     :when (= 2 (count number-values))]
    (apply * number-values)))

(defn calculate-sum [input sym-re three-rows->numbers]
  (transduce
    (comp
      cat
      (map-indexed (token-data-fn sym-re))
      (map #(x/into {} (x/by-key (x/into [])) %))
      (x/partition 3 1)
      (mapcat three-rows->numbers))
    + 0
    [[""] input [""]]))

(defn part1 [input]
  (calculate-sum input #"[^.\d]" numbers-next-to-symbols))

(defn part2 [input]
  (calculate-sum input #"\*" product-next-to-symbols-next-to-2-numbers))

(comment

  (= 4361 (part1 (util/input-seq "data/y23/day3/test.txt")))
  (part1 (util/input-seq "data/y23/day3/input.txt")) ;; 514969

  (= 467835 (part2 (util/input-seq "data/y23/day3/test.txt")))
  (part2 (util/input-seq "data/y23/day3/input.txt")) ;; 78915902

  )
