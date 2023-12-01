(ns day1 (:require [clojure.string :refer [index-of split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)]
    (loop [input input nums []]
      (if-not (seq input)
        (println (apply + nums))
        (let [row (first input)
              elems (split row #"")
              digits (filter int? (map read-string elems))
              n (+ (* 10 (first digits)) (last digits))]
          (recur (rest input) (conj nums n)))))))

(solve-1 "test-input")
(solve-1 "input")

(def digits 
  {"one"   1 "1" 1 
   "two"   2 "2" 2
   "three" 3 "3" 3
   "four"  4 "4" 4
   "five"  5 "5" 5
   "six"   6 "6" 6
   "seven" 7 "7" 7
   "eight" 8 "8" 8
   "nine"  9 "9" 9})

;; find all indices of elem in string
;; returns list of [idx num] pairs (idx is used for sort later)
(defn index-of-all [string elem]
  (loop [pos 0 indices []]
    (if (> pos (count string))
      indices
      (let [idx (index-of string elem pos)]
        (if (nil? idx)
          indices
          (recur (inc idx) (conj indices [idx (digits elem)])))))))

;; find all numbers in a string which are either words or digits
;; returns flat list of [idx resolved-number-value]
(defn find-all-indexed-numbers [string]
  (loop [candidates (keys digits) found []]
    (if-not (seq candidates)
      (sort (remove empty? found))
      (let [i (index-of-all string (first candidates))]
        (recur (rest candidates) (apply conj found i))))))

;; solve-2 find all indices for digit-words and digits themselves
(defn solve-2 [input-file]
  (let [input (input->vec input-file)]
    (loop [input input nums []]
      (if-not (seq input)
        (println (apply + nums))
        (let [row (first input)
              indices (find-all-indexed-numbers row)
              f (first indices)
              l (last indices)
              n (+ (* 10 (last f)) (last l))]
          (recur (rest input) (conj nums n)))))))

(solve-2 "test-input-2")
(solve-2 "input")
