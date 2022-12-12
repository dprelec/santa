(ns day8 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; split row into parsed vector of numbers
(defn split-records [row] (vec (map read-string (split row #""))))

;; elem return element of matrix `m` located at [r, c]
(defn elem [m r c] (nth (nth m r) c))

;; bigger returns true if element `e` is bigger from all the elements of
;; `coll`
(defn bigger [e coll]
  (let [bigger? (partial #(> e %1))]
    (empty? (remove bigger? coll))))

(defn solve1 [input-file]
  (let [input (input->vec input-file)
        ;; input as list of parsed vectors
        input (vec (map split-records input))
        ;; amount of rows
        rows (count input)
        ;; amount of columns
        cols (count (first input))
        ;; all middle row indices for candidates
        xrange (range 1 (dec rows))
        ;; all middle column indices for candidates
        yrange (range 1 (dec cols))
        ;; inital count: number of rows + number of cols - 2 (for rows already
        ;; calculated)
        edges (+ (* 2 rows) (* 2 (- cols 2)))
        ;; all seen tops
        seen (filter true?
                     (for [x xrange y yrange]
                       (let [;; current row
                             row (nth input x)
                             ;; current column
                             col (vec (map #(nth % y) input))
                             ;; candidate for comparison
                             candidate (elem input x y)
                             ;; elements on the left of candidate
                             left (reverse (take y row))
                             ;; elements on the right of candidate
                             right (drop (inc y) row)
                             ;; elements on the top of candidate
                             top (reverse (take x col))
                             ;; elements on the bottom of candidate
                             bottom (drop (inc x) col)
                             ;; candidate is bigger that at least one collection
                             visible (or (bigger candidate left)
                                         (bigger candidate right)
                                         (bigger candidate top)
                                         (bigger candidate bottom))]
                         visible)))]
    (+ edges (count seen))))

(println "Test 1:" (solve1 "test-input"))
(println "Solution 1:" (solve1 "input"))

;; take-until collects elements from coll until (f elem e) returns true.
;; It then returns last element inspected with all previous resuls.
;; `f` is a function of two arguments.
(defn take-until [e coll f]
  (loop [coll coll res []]
    (if-not (seq coll)
      res
      (if (f (first coll) e)
        (conj res (first coll))
        (recur (rest coll) (conj res (first coll)))))))

;; count number of visible items in `coll` as viewed from `e`.
(defn score [e coll] (count (take-until e coll >=)))

(defn solve2 [input-file]
  (let [input (input->vec input-file)
        ;; input as list of parsed vectors
        input (vec (map split-records input))
        ;; amount of rows
        rows (count input)
        ;; amount of columns
        cols (count (first input))
        ;; all middle row indices for candidates
        xrange (range 1 (dec rows))
        ;; all middle column indices for candidates
        yrange (range 1 (dec cols))
        ;; all seen tops
        scores (apply max
                      (for [x xrange y yrange]
                        (let [;; current row
                              row (nth input x)
                              ;; current column
                              col (vec (map #(nth % y) input))
                              ;; candidate for comparison
                              candidate (elem input x y)
                              ;; elements on the left of candidate
                              left (reverse (take y row))
                              ;; elements on the right of candidate
                              right (drop (inc y) row)
                              ;; elements on the top of candidate
                              top (reverse (take x col))
                              ;; elements on the bottom of candidate
                              bottom (drop (inc x) col)
                              ;; calculate total score for a tree
                              total-score (* (score candidate left)
                                             (score candidate right)
                                             (score candidate top)
                                             (score candidate bottom))]
                          total-score)))]
    scores))

(println "Test 2:" (solve2 "test-input"))
(println "Solution 2:" (solve2 "input"))
