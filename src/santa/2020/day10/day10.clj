(ns santa.2020.day10
 (:require [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] 
  (into [] (map read-string (split-lines (slurp input-file)))))

(defn differences [input]
  (let [r (group-by identity (map #(- (second %) (first %)) (partition 2 1 (sort input))))] 
    (* (inc (count (r 1))) (inc (count (r 3))))))

;; solution to part 1
(println (differences (input->vec "input")))

(defn max+3 [c] (+ 3 (apply max c)))

;; add 0 and max+3 to collection and return it sorted
(defn create-chain [coll] (sort (conj (conj coll (max+3 coll)) 0)))

;; map of chain size offset by one to number of combinations
(def cm {2 1 3 2 4 4 5 7})

(defn diff-pair [p] (- (second p) (first p)))

(defn calculate-combinations [chain]
  ;; partition chain into pairs: [(0 1) (1 2) ...]
  ;; calculate diff between pair elems: [1 1 1 1 3 ...]
  ;; group by that diff ( (1 1 1 1)  (3 ...) )
  ;; select groups with ones
  ;; map to count(number of ones) + 1 (number of ones is one less than the
  ;; interval the ones were calculated from)
  ;; map with combination map
  ;; multiply all
  (let [pairs (partition 2 1 chain)
        diffs (map diff-pair pairs)
        grouped (partition-by identity (map diff-pair pairs))
        ones (filter #(= (first %) 1) grouped)
        combs (map #(cm (inc (count %))) ones)]
    (apply * combs)))

;; solution to part 2
(println (calculate-combinations (create-chain (input->vec "input"))))
