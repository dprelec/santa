(ns day13 (:require [clojure.string :refer [replace split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn score [what point]
  (if (= what "lose")
    (- point)
    point))

;(defmacro merge-mapv [mv] `(merge-with merge ~@mv))

(defn score-pairs [scores members member]
  (let [candidates (disj members member)
        matches (for
                  [candidate candidates]
                  {:key (sort [member candidate])
                   :score (+ ((scores member) candidate)
                             ((scores candidate) member))})
        sorted (sort-by :score matches)
        sorted (reverse sorted)]
    sorted))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        input (map #(split % #" ") input)
        scoremaps (for [row input]
                    {(first row) {(replace (last row) "." "")
                                  (score (nth row 2) (read-string (nth row 3)))}})
        scores (apply merge-with merge scoremaps)
        members (set (keys scores))
        ;; score every pair
        matches (map #(score-pairs scores members %) members)
        matches (apply concat matches)
        matches (distinct matches)
        sorted-all (reverse (sort-by :score matches))]
    (doseq [s sorted-all]
      (println "m:" s))    
      sorted-all))

(println (solve-1 "test-input"))

(println (solve-1 "input"))
