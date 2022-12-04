(ns santa.2020.day20.day20
  (:require [clojure.string :refer [join split-lines]]))

(def test-input-1
  ["..##.#..#." 
   "##..#....." 
   "#...##..#." 
   "####.#...#" 
   "##.##.###."
   "##...#.###" 
   ".#.#.#..##" 
   "..#....#.." 
   "###...#.#." 
   "..###..###"])

(defn bin->dec [b] (Integer/parseInt b 2))

(defn char->bin
  [c]
  (case c
    \# 1
    \. 0))

(defn row->dec [row] (bin->dec (join (map char->bin row))))

(defn top [rows] (first rows))
(defn bottom [rows] (last rows))
(defn left [rows] (join (for [r (reverse rows)] (first r))))
(defn right [rows] (join (for [r rows] (last r))))

(defn tile-sizes
  [rows]
  {:left-cw (row->dec (left rows)),
   :right-cw (row->dec (right rows)),
   :top-cw (row->dec (top rows)),
   :bottom-cw (row->dec (bottom rows)),
   :left-ccw (row->dec (reverse (left rows))),
   :right-ccw (row->dec (reverse (right rows))),
   :top-ccw (row->dec (reverse (top rows))),
   :bottom-ccw (row->dec (reverse (bottom rows)))})

(println (tile-sizes test-input-1))
(println (row->dec (first test-input-1)))
(println (row->dec (last test-input-1)))
(println (row->dec (reverse (first test-input-1))))
(println (row->dec (reverse (last test-input-1))))
(println (left test-input-1))
