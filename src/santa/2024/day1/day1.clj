(ns day1 (:require [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn transpose [matrix] (apply mapv vector matrix))

(defn pair-diff [[x y]] (Math/abs (- x y)))

(defn input->matrix [input-file]
  (->> input-file
       input->vec
       (map #(map read-string (re-seq #"\d+" %)))
       transpose))

(defn solve1 [input-file]
  (let [input (->> input-file
                   input->matrix
                   (mapv sort)
                   ;; convert into pairs
                   (apply map vector))
        diffs (map pair-diff input)
        sum (apply + diffs)]
    sum))

(println "Test 1:" (solve1 "test-input"))
(println "Solution 1:" (solve1 "input"))

(defn solve2 [input-file]
  (let [input (->> input-file
                   input->matrix )
        left (first input)
        right (frequencies (second input))
        score (map #(* % (right % 0)) left)
        sum (apply + score)]
    sum))

(println "Test 2:" (solve2 "test-input"))
(println "Solution 2:" (solve2 "input"))
