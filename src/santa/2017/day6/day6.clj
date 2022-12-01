(ns day6 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        input (first input)
        elems (split input #"\t")
        nums (vec (map read-string elems))]
    #{nums}))

;; indexed-max-elem returns [element, index] of maximum element in nums.
(defn indexed-max [nums]
  (let [;; find max element
        max-elem (apply max nums)
        ;; zip nums with indices
        indexed (map vector nums (range (count nums)))
        ;; filter by max element in first position
        max? (fn [[c & _]] (= max-elem c)) 
        match (filter max? indexed)
        ;; first match found returned
        match (first match)]
    match))
    

;; fix max element
;; set this to zero
;; redistribute contents of it to all elements starting from the next
;; when done, see if we have the resulting vector in set
;; if so, return total count
;; otherwise repeat from step 1
(defn count-distribution-cycles [nums]
  (loop [cycle-count 0 nums nums seen #{nums}]
    (let [max-elem (indexed-max nums)])))
    


(println (solve-1 "input"))
