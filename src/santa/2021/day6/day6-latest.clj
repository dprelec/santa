(ns santa.2021.day6
  (:require [clojure.string :refer [join split]]))

(defn read-input [file] 
  (map read-string (split (slurp file) #",")))

(defn decrease [c]
  (if (= c 0)
    6
    (dec c)))

(defn solve1 [file]
  (loop [day 0 input (read-input file) old input sum 0]
    (println day ":" (join "" input))
    (println day ":" :diff (- (count input) (count old)) :sum sum)
    (println sum)
    (println :sum sum :diff (- (count input) sum) :day day)
    (if (>= day 80)
      (count input)
      (let [old input
            zeros (count (filter zero? input))
            input (map decrease input)]
        (if (zero? zeros)
          (recur (inc day) input old sum)
          (recur (inc day) (concat input (repeat zeros 8)) old (+ sum zeros)))))))

(println (solve1 "test-input")) ;; 5934
