(ns day2 (:require 
           [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn min-max 
  "Return min and max value for each row in rows."
  [rows]
  (into [] (for [row rows] [(apply min row) (apply max row)])))

(defn abs-diff 
  "Return absolute difference of first elem from the second."
  [v]
  (Math/abs (- (first v) (second v))))

(defn even-division
  "Find even divisors in vector: numbers where first evenly divides the second.
   Return result of that division."
  [v]
  (loop [divisors [] v (sort > v)]
    (if-not (seq v)
      (first divisors)
      (let [d (filter integer? (map #(/ (first v) %) (rest v)))]
        (if (seq d)
          (recur (conj divisors (first d)) (rest v))
          (recur divisors (rest v)))))))

(defn solve-1 [input-file]
  (let [;; read input
        sheet (input->vec input-file)
        ;; split input strings
        sheet (map #(split % #"\t") sheet)
        ;; convert to number matrix
        sheet (mapv #(map read-string %) sheet)
        ;; extract min and max of each row
        minmax (min-max sheet)
        ;; calculate differences of first - second element of each
        diffs (map abs-diff minmax)]
    (apply + diffs)))

(println (solve-1 "input"))
        
(defn solve-2 [input-file]
  (let [sheet (input->vec input-file)
        sheet (map #(split % #"\t") sheet)
        sheet (mapv #(map read-string %) sheet)
        divs (map even-division sheet)]
    (apply + divs)))

(println (solve-2 "input"))

