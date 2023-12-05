(ns imrekoszo.advent.y23.day3
  (:require
    [imrekoszo.advent.util :as util]
    [net.cgrand.xforms :as x])
  (:import
    (java.util.regex Matcher)))

(set! *warn-on-reflection* true)

(defn adjacent-coords
  "Returns the set of coordinates surrounding the current match of matcher.

  Coordinate format is [row-index character-position]"
  [row-idx ^Matcher matcher]
  (let [s (.start matcher)
        e (.end matcher)
        char-range (set (range s e))
        char-around (range (dec s) (inc e))]
    (x/into #{}
      (x/for
        [c _ r (range (dec row-idx) (+ 2 row-idx))
         :when (not (and (= row-idx r) (char-range c)))]
        [r c])
      char-around)))

(defn parse-row
  "Returns token information about a row.

  [{:nums [{:value 123 :adjacent #{[1 2] [1 3] ...}} ...]
    :syms [{:sym \"=\" :coords [2 5]} ...]}
   ...]"
  [row-idx row]
  (x/into {:syms [] :nums []}
    (x/by-key
      #(if (:sym %) :syms :nums)
      (x/into []))
    (let [matcher (re-matcher #"([^.\d])|(?:\d+)" row)]
      (iteration
        (fn [_] (re-find matcher))
        :vf (fn [[token sym]]
              (if (some? sym)
                {:sym token
                 :coords [row-idx (.start matcher)]}
                {:value (parse-long token)
                 :adjacent (adjacent-coords row-idx matcher)}))))))

(defn numbers-next-to-symbols
  "Looks at data from 3 adjacent rows and returns an eduction of the numbers
   from the middle row that are adjacent to a symbol on any row"
  [three-rows]
  (x/for
    [{:keys [value adjacent]} (:nums (second three-rows))
     :let [adjacent-to-current-num? #(adjacent (:coords %))]
     :when (x/some
             (comp (mapcat :syms) (keep adjacent-to-current-num?))
             three-rows)]
    value))

(defn products-next-to-symbols-next-to-2-numbers
  "Looks at data from 3 adjacent rows and returns an eduction of the products
  of the numbers (on any of the rows) that are next to a symbol on the middle
  row, which is next to exactly 2 numbers"
  [three-rows]
  (x/for
    [sym (:syms (second three-rows))
     :when (= "*" (:sym sym))
     :let [sym-coord (:coords sym)
           value-adjacent-to-current-sym #(when ((:adjacent %) sym-coord) (:value %))
           adjacent-values (x/into []
                             (comp
                               (mapcat :nums)
                               (keep value-adjacent-to-current-sym)
                               (take 3))
                             three-rows)]
     :when (= 2 (count adjacent-values))]
    (reduce * adjacent-values)))

(defn solve [input]
  (x/some
    (comp
      cat
      (map-indexed parse-row)
      (x/partition 3 1)
      (x/transjuxt
        {:part1 (comp (mapcat numbers-next-to-symbols) (x/reduce +))
         :part2 (comp (mapcat products-next-to-symbols-next-to-2-numbers) (x/reduce +))}))
    [[""] input [""]]))

(comment

  (= (solve (util/input-seq "data/y23/day3/test.txt"))
    {:part1 4361, :part2 467835})

  (solve (util/input-seq "data/y23/day3/input.txt")) ;; {:part1 514969, :part2 78915902}
  )
