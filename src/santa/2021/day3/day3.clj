(ns santa.2021.day3
  (:require [clojure.string :refer [join split split-lines]]))

;; read input lines
(defn input-lines [file] (split-lines (slurp file)))

;; convert input lines into matrix
(defn lines->matrix [lines] (map #(mapv read-string (split % #"")) lines))

;; transpose matrix
(defn transpose [matrix] (apply mapv vector matrix))

;; return frequencies of each row in transposed matrix
(defn matrix-freqs [matrix] (map frequencies (transpose matrix)))

;; return most-common number from the frequency map
(defn most-common [freq] (if (> (freq 0) (freq 1)) 0 1))

;; return least-common number from the frequency map
(defn least-common [freq] (if (> (freq 0) (freq 1)) 1 0))

;; convert sequence of binary numbers to decimal
(defn seq->dec [s] (Integer/parseInt (join (map str s)) 2))

(defn solve1 [file]
  (let [freqs (matrix-freqs (lines->matrix (input-lines file)))
        most (map most-common freqs)
        least (map least-common freqs)
        gamma (seq->dec most)
        epsilon (seq->dec least)]
    (* gamma epsilon)))

(println (solve1 "input"))

(defn rating [matrix commonfn]
  (loop [pos 0 matrix matrix]
    (if (= 1 (count matrix))
      (first matrix)
      (let [common (commonfn (nth (map frequencies (transpose matrix)) pos))]
        (recur (inc pos) (filter #(= common (nth % pos)) matrix))))))

(defn solve2 [file]
  (let [matrix (lines->matrix (input-lines file))
        oxy-rate (seq->dec (rating matrix most-common))
        co2-rate (seq->dec (rating matrix least-common))]
    (* oxy-rate co2-rate)))

(println (solve2 "input"))
