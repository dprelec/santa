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
                   ;; sort rows 
                   (mapv sort)
                   ;; convert into pairs
                   (apply map vector))
        ;; calculate difference
        diffs (map pair-diff input)
        ;; sum all
        sum (apply + diffs)]
    (println sum)))

(solve1 "test-input")
(solve1 "input")

(defn solve2 [input-file]
  (let [input (->> input-file
                   input->vec
                   (map #(map read-string (re-seq #"\d+" %)))
                   transpose)
        left (first input)
        right (frequencies (second input))
        score (map #(* % (right % 0)) left)
        sum (apply + score)]
    (println sum)))


(solve2 "test-input")
(solve2 "input")
