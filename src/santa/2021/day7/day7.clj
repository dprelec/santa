(ns santa.2021.day7
  (:require [clojure.string :refer [split split-lines]]))

(defn read-input [file] 
  (map read-string (split (slurp file) #",")))

(defn median [x]
  (let [n (count x)]
    (if (odd? n)
      (nth x (/ n 2))
      (let [n1 (dec (/ n 2))
            n2 (inc n1)
            m1 (nth x n1)
            m2 (nth x n2)
            a (/ (+ m1 m2) 2)]
        (float a)))))

(defn average [x]
  (float (/ (float (apply + x))
            (count x))))

(defn solve1 [file]
  (let [input (read-input file)
        med (median (sort input))]
    (apply + (map #(Math/abs (- % med)) input))))

(defn fuel [n avg] (apply + (range 1 (inc (Math/abs (- n avg))))))

(defn solve2 [file]
  (let [input (read-input file)
        start (apply min input)
        end (apply max input)]
    (loop [candidate (range start (inc end)) sums []]
      (if-not (seq candidate)
        (apply min sums)
        (recur (rest candidate) (conj sums (apply + (map #(fuel % (first candidate)) input))))))))

(println (solve1 "test-input"))
(println (solve1 "input"))

(println (solve2 "test-input"))
(println (solve2 "input"))
