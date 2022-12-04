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
    ;(println sorted)
    [(first sorted) (second sorted)]))

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
        scores (map :score matches)]
    (doseq [s matches]
      (println "m:" (:key s)))
    (apply + scores)))

;; scorese merged
(def test-data
  {"Alice" {"Bob" 54, "Carol" -79, "David" -2},
   "Bob" {"Alice" 83, "Carol" -7, "David" -63},
   "Carol" {"Alice" -62, "Bob" 60, "David" 55},
   "David" {"Alice" 46, "Bob" -7, "Carol" 41}})

;; scoremap
(def test-data-2
  '({Alice {Bob 54}}
    {Alice {Carol -79}}
    {Alice {David -2}}
    {Bob {Alice 83}}
    {Bob {Carol -7}}
    {Bob {David -63}}
    {Carol {Alice -62}}
    {Carol {Bob 60}}
    {Carol {David 55}}
    {David {Alice 46}}
    {David {Bob -7}}
    {David {Carol 41}}))


(println (solve-1 "test-input"))

(println (solve-1 "input"))
