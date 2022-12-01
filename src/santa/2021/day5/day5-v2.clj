(ns santa.2021.day5
  (:require [clojure.string :refer [join split split-lines]]
            [clojure.pprint :refer [pprint]]))

;; read input lines
(defn input-lines [file] (split-lines (slurp file)))

;; parse input lines in the format:
;; 
;;   x1,y1 -> x2,y1
;;
;; into:
;; 
;;   ({:x1 x1 :y1 y1 :x2 x2 :y2 y2}, ...)
;;
(defn parse-input [lines]
  (loop [lines lines coords []]
    (if-not (seq lines)
      coords
      (let [line (first lines)
            pair (split line #"\s+->\s+")
            nums (map #(split % #",") pair)
            nums (flatten nums)
            nums (map read-string nums)
            nums (vec nums)]
        (recur (rest lines) (conj coords nums))))))

;; line range returns list of numbers starting from smaller one up to the
;; bigger and and including it:
;;
;; [1 3] -> (1 2 3)
;; [6 2] -> (2 3 4 5 6)
(defn line-range [x y] (if (< x y) (range x (inc y)) (range y (inc x))))

(defn line-range-diag [x y] (if (< x y) (range x (inc y)) (reverse (range y (inc x)))))

;; horiz-or-vert-line returns true if either x1 = x2 or y1 = y2.
(defn horiz-or-vert-line [coords]
  (or 
    (= (coords 0) (coords 2))
    (= (coords 1) (coords 3))))

;; points returns list of points in the straight line:
;;
;; [9 4 3 4] -> ([3 4] [4 4] [5 4] [6 4] [7 4] [8 4] [9 4])
;; [0 9 5 9] -> ([0 9] [1 9] [2 9] [3 9] [4 9] [5 9])
(defn points [coords]
  (for [x (line-range (coords 0) (coords 2))
        y (line-range (coords 1) (coords 3))]
    [x y]))

(defn diagonal-points [coords]
  (let [x (line-range-diag (coords 0) (coords 2))
        y (line-range-diag (coords 1) (coords 3))]
    (into [] (zipmap x y))))
  
(defn points2 [coords]
  (if (horiz-or-vert-line coords)
    (points coords)
    (diagonal-points coords)))

(defn solve1 [file]
  (let [lines (input-lines file)
        input (parse-input lines)
        input (filter horiz-or-vert-line input)
        pairs (apply concat (map points input))
        freqs (frequencies pairs)
        multi (filter #(> (val %) 1) freqs)]
    (count multi)))

(defn solve2 [file]
  (let [lines (input-lines file)
        input (parse-input lines)
        pairs (apply concat (map points2 input))
        freqs (frequencies pairs)
        multi (filter #(> (val %) 1) freqs)]
    (count multi)))

(pprint (solve1 "test-input"))
(pprint (solve1 "input"))
(pprint (solve2 "test-input"))
(pprint (solve2 "input"))
