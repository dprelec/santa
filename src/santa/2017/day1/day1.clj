(ns day1 (:require [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn first=second [pair] (= (first pair) (second pair)))

(defn solve-1 [input-file]
  (let [;; read input as vector
        input (input->vec input-file)
        ;; take first row and split into chars
        nums (split (first input) #"")
        ;; parse chars as integers
        nums (map read-string nums)
        ;; create pairs out of numbers
        pairs (partition-all 2 1 nums)
        ;; take pairs where first elem is equal to second
        pairs (filter first=second pairs)
        ;; take first element of each pair
        matches (map first pairs)
        ;; sum them all
        sum (apply + matches)]
    ;; if last number matches the first number of the input
    (if (= (first nums) (last nums))
      (+ sum (first nums))
      sum)))

;; solve-2 solves part 2 as follows:
;; * take original collection, eg. [1 2 1 2]
;; * split it in half: [1 2] [1 2]
;; * reverse order of these parts: [ [second-part] [first-part] ]
;; * zip with the original collection (haskell zip):
;;   [ [1 1] [2 2] [1 1] [2 2] ]
;; * take those where first element is equal to second
;; * sum all first elements of matching vectors
(defn solve-2 [input-file]
  (let [;; read input as vector
        input (input->vec input-file)
        ;; take first row and split into chars
        nums (split (first input) #"")
        ;; parse chars as integers
        nums (map read-string nums)
        ;; take index at middle of nums
        half (/ (count nums) 2)
        ;; take last half of the collection
        last-nums (drop half nums)
        ;; take first half of the collection
        first-nums (take half nums)
        ;; concatenate them as part of resulting vector!
        nums2 (apply concat [last-nums first-nums])
        ;; zip the collections haskell-wise
        zipped (map vector nums nums2)
        ;; collect matches where first elem is eq to second
        pairs (filter first=second zipped)
        ;; take first elemet of every pair
        matches (map first pairs)
        ;; sum first elems of them all
        sum (apply + matches)]
    sum))

(println "Solution 1:" (solve-1 "input"))
(println "Solution 2:" (solve-2 "input"))



