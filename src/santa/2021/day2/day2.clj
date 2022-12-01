(ns santa.2021.day2
  (:require [clojure.string :refer [split split-lines]]))

(defn input-lines [file] (split-lines (slurp file)))

(defn calculate-position-and-depth [commands]
  (loop [position 0 depth 0 commands commands]
    (if-not (seq commands)
      (* position depth)
      (let [command (split (first commands) #" ")
            direction (first command)
            step (read-string (second command))]
        (case direction
          "forward" (recur (+ position step) depth (rest commands))
          "down" (recur position (+ depth step) (rest commands))
          "up" (recur position (- depth step) (rest commands)))))))

(defn calculate-aimed-position-and-depth [commands]
  (loop [position 0 depth 0 aim 0 commands commands]
    (if-not (seq commands)
      (* position depth)
      (let [command (split (first commands) #" ")
            direction (first command)
            step (read-string (second command))]
        (case direction
          "down" (recur position depth (+ aim step) (rest commands))
          "up" (recur position depth (- aim step) (rest commands))
          "forward" (recur (+ position step) (+ depth (* aim step)) aim (rest commands)))))))

(defn solve-part1 [file] (calculate-position-and-depth (input-lines file)))
(defn solve-part2 [file] (calculate-aimed-position-and-depth (input-lines file)))

(println (solve-part1 "input"))
(println (solve-part2 "input"))

