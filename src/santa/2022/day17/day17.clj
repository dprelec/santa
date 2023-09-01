(ns day17 (:require [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(def shape-hbar [[1 1 1 1]])

(def shape-cross [[0 1 0]
                  [1 1 1]
                  [0 1 0]])

(def shape-l [[0 0 1]
              [0 0 1]
              [1 1 1]])

(def shape-vbar [[1]
                 [1]
                 [1]
                 [1]])

(def shape-brick [[1 1]
                  [1 1]])


(defn move-shape [shape direction]
  (case direction
    "<" (apply map dec shape)
    ">" (apply map inc shape)))

(println (move-shape shape-hbar "<"))
(println (move-shape shape-hbar ">"))

(println (move-shape shape-l "<"))

(println (move-shape shape-l ">"))

