(ns day1 (:require [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn string->int [s] (if (= s "") 0 (read-string s)))

(defn solve-1 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; convert empty line to zeros
        input (map string->int input)
        ;; group entries separated by zero
        groups (partition-by zero? input)
        ;; remove zero input
        groups (filter #(not (zero? (first %))) groups)
        ;; calculate sum of every group
        sums (map #(apply + %) groups)
        ;; result
        max-sum (apply max sums)]
    max-sum))

(println (solve-1 "test-input"))
(println (solve-1 "input"))

(defn solve-2 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; convert empty line to zeros
        input (map string->int input)
        ;; group entries separated by zero
        groups (partition-by zero? input)
        ;; remove zero input
        groups (filter #(not (zero? (first %))) groups)
        ;; calculate sum of every group
        sums (map #(apply + %) groups)
        ;; sort by sum
        biggest (sort > sums)
        ;; result
        result (apply + (take 3 biggest))]
    result))

(println (solve-2 "test-input"))
(println (solve-2 "input"))
