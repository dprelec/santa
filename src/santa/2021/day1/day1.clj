(ns santa.2021.day1
  (:require [clojure.string :refer [split-lines]]))

(defn input->numvec [file] (map read-string (split-lines (slurp file))))

(defn count-increases [nums] (count (filter #(< % 1) (map #(apply / %) (partition 2 1 nums)))))

(defn solve-part1 [file] (println (count-increases (input->numvec file))))

(defn solve-part2 [file]
  (let [nums (input->numvec file)
        sums (map #(apply + %) (partition 3 1 nums))]
    (println (count-increases sums))))

(defn solve-test-part1 []
  (let [test-input [199 200 208 210 200 207 240 269 260 263]]
    (println (count-increases test-input))))

(solve-test-part1)
(solve-part1 "input")
(solve-part2 "input")

