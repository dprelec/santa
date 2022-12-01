(ns day5 (:require [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-1 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; parse input as integers into vector!
        ;; (for update function to work)
        input (vec (map read-string input))]
    (loop [jumps 0 loc 0 instructions input]
      (println instructions)
      ;; if we are out of bounds, return number of jumps
      (if (>= loc (count instructions))
        jumps
        (let [next-jump (instructions loc)]
          (if (> next-jump (count instructions))
            jumps
            (recur (inc jumps)
                   (+ (instructions loc) loc)
                   (update instructions loc inc))))))))

;(println "Test 1:" (solve-1 "test-input"))
;(println "Solution 1:" (solve-1 "input"))

(defn next-offset [loc] (if (>= loc 3) -1 1))

(defn solve-2 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; parse input as integers into vector!
        ;; (for update function to work)
        input (vec (map read-string input))]
    (loop [jumps 0 loc 0 instructions input]
      ;; if we are out of bounds, return number of jumps
      (if (>= loc (count instructions))
        jumps
        (let [next-jump (instructions loc)]
          (if (> next-jump (count instructions))
            jumps
            (recur (inc jumps)
                   (+ (instructions loc) loc)
                   (assoc instructions loc (+ (instructions loc) (next-offset (instructions loc)))))))))))

(println "Test 2:" (solve-2 "test-input"))
(println "Solution 2:" (solve-2 "input"))
