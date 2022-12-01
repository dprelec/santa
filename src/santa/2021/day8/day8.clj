(ns day8
  (:require [clojure.string :refer [join split split-lines]]))

(defn read-input [file] (split-lines (slurp file)))

(defn count-instances [sm]
  (+ (sm 2) (sm 4) (sm 3) (sm 7)))

(defn solve1 [file]
  (let [input (read-input file)]
    (loop [sum {} lines input]
      (if-not (seq lines)
        (count-instances sum)
        (let [parts (split (first lines) #"\s*\|\s*")
              output (split (second parts) #"\s+")]
          (recur (merge-with + sum (frequencies (map count output))) (rest lines)))))))

(defn all [sm]
  (count [(sm 2) (sm 4) (sm 3) (sm 7)]))

(defn construct [segments]
  (loop [display {} segments segments]
    (if-not (seq segments)
      display
      (let [segment (split (first segments) #"")
            remap 
            (condp = (count segment)
              2 {(nth segment 0) "c" (nth segment 1) "f"} ;; 1
              4 {(nth segment 0) "b" (nth segment 1) "c" (nth segment 2) "d" (nth segment 3) "f"} ;; 4
              3 {(nth segment 0) "a" (nth segment 1) "c" (nth segment 2) "f"} ;; 7
              7 {(nth segment 0) "a" (nth segment 1) "b"
                 (nth segment 2) "c" (nth segment 3) "d"
                 (nth segment 4) "e" (nth segment 5) "f"
                 (nth segment 6) "g"}
              {})]
        (recur (merge display remap) (rest segments))))))

(defn solve2 [file]
  (let [input (read-input file)]
    (loop [sum {} lines input]
      (if-not (seq lines)
        (count-instances sum)
        (let [parts (split (first lines) #"\s*\|\s*")
              input (split (first parts) #"\s+")
              output (split (second parts) #"\s+")
              constr (construct input)]
          (println :c constr :from input :to (map #(join "" (replace constr (split % #""))) input))
          (recur (merge-with + sum (frequencies (map count output))) (rest lines)))))))

(println (solve2 "test-input"))
