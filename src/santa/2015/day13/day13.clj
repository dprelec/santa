(ns day13 (:require [clojure.string :refer [replace split split-lines]]))

(defn swaps [n]
  (if (= n 1)
    ()
    (let [base (swaps (dec n))
          extras (if (odd? n) (repeat (dec n) 0) (range (dec n)))]
      (concat
       base
       (mapcat (fn [x] (cons [x (dec n)] base)) extras)))))

(defn permutations [v]
  (reductions
   (fn [a [i j]] (assoc a i (a j) j (a i)))
   v
   (swaps (count v))))

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
                  {[member candidate]
                   (+ ((scores member) candidate)
                      ((scores candidate) member))})]
    matches))

(defn get-score [scoremap from to] (scoremap [from to]))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        input (map #(split % #" ") input)
        ;; convert into nest map: {"Alice" {"Bob" 123}}
        scoremaps (for [row input]
                    {(first row) {(replace (last row) "." "")
                                  (score (nth row 2) (read-string (nth row 3)))}})
        scores (apply merge-with merge scoremaps)
        ;; member names
        members (set (keys scores))
        ;; score every pair
        matches (map #(score-pairs scores members %) members)
        ;; normalize list into one map
        matches (into {} (for [m (apply concat matches)] m))
        names (vec (sort members))
        perms (permutations names)]
    (loop [perms perms sum []]
      (if-not (seq perms)
        (first (sort > (map #(apply + %) sum)))
        (let [perm (first perms)
              part (partition 2 1 perm)
              lost (list (last perm) (first perm))
              pairs (conj part lost)
              scores (into [] (for [pair pairs] (get-score matches (first pair) (second pair))))]
          (recur (rest perms) (conj sum scores)))))))

(println (solve-1 "test-input"))
(println (solve-1 "input"))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)
        input (map #(split % #" ") input)
        ;; convert into nest map: {"Alice" {"Bob" 123}}
        scoremaps (for [row input]
                    {(first row) {(replace (last row) "." "")
                                  (score (nth row 2) (read-string (nth row 3)))}})
        scores (apply merge-with merge scoremaps)
        ;; member names
        members (set (keys scores))
        ;; score every pair
        matches (map #(score-pairs scores members %) members)
        ;; normalize list into one map
        matches (into {} (for [m (apply concat matches)] m))
        names (vec (sort members))
        names (conj names "Darko")
        perms (permutations names)]
    (loop [perms perms sum []]
      (if-not (seq perms)
        (first (sort > (map #(apply + %) sum)))
        (let [perm (first perms)
              part (partition 2 1 perm)
              lost (list (last perm) (first perm))
              pairs (conj part lost)
              scores (into [] (for [pair pairs]
                                (let [s (matches [(first pair) (second pair)])]
                                  (if (nil? s) 0 s))))]
          (recur (rest perms) (conj sum scores)))))))

(println (solve-2 "input"))
