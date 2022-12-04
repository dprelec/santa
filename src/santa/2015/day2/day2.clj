(ns santa.2015.day2
  (:require [clojure.string :refer [split split-lines]]))

(defn input->numbers [input]
  (map #(map read-string (split % #"x")) input))

;; area of the smallest side
(defn smallest-area [l w h]
  (min (* l w) (* l h) (* w h)))

(defn surface-area [l w h]
  (+ (* 2 l w) (* 2 w h) (* 2 h l) (smallest-area l w h)))

(defn total-surface-area [input]
  (apply + (for [[l w h] (input->numbers input)]
             (surface-area l w h))))

;; solution to part 1
(println (total-surface-area (split-lines (slurp "input-2015-2"))))

(defn bow-length [l w h] (* l w h))

(defn shortest-distance [l w h] 
  (let [[l w h] (sort [l w h])]
    (+ l l w w)))

(defn ribbon-length [l w h]
  (+ (bow-length l w h) (shortest-distance l w h)))

(defn total-ribbon-length [input]
  (apply + (for [[l w h] (input->numbers input)]
             (ribbon-length l w h))))

; solution to part 2
(println (total-ribbon-length (split-lines (slurp "input-2015-2"))))

