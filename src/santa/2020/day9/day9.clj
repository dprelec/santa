(ns santa.2020.day9
 (:require [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] 
  (into [] (map read-string (split-lines (slurp input-file)))))

(defn sum-all [nums] (into #{} (for [x nums y nums :while (not= x y)] (+ x y))))

(defn partition-window [input w] (filter #(= w (count %)) (partition-all w 1 input)))

(defn first-non-summed [numbers window-size]
  ;; 1) partition all numbers into sliding-window sublists
  ;; 2) take only window-size slices
  ;; 3) combine them into [1,2], [2,3] etc
  ;; 4) sum numbers in slices into set
  ;; 5) check membership
  ;; 6) return first candidate
  (let [slices (partition-window numbers window-size)
        combs (partition-all 2 1 slices)]
    (first (filter #(= false (:c %)) (for [[l n] combs] {:n (last n) :c (contains? (sum-all l) (last n))})))))

;; solution to part 1
(println (:n (first-non-summed (input->vec "test-input") 5)))
(println (:n (first-non-summed (input->vec "input") 25)))


(defn find-weaknes [input dest-sum max-window]
  (first (flatten (remove empty? 
          (for [w (range 2 max-window)] 
            (filter #(= (:sum %) dest-sum)
                   (map (fn [c] {:sum (apply + c) :res (+ (apply min c) (apply max c))})
                        (partition-window input w))))))))

;; solution to part 2
;; max-window value discovered by accident
(println (:res (find-weaknes (input->vec "input") 23278925 20)))

