(ns santa.2021.day6
  (:require [clojure.string :refer [split]]))

(defn read-input [file] 
  (map read-string (split (slurp file) #",")))

(defn decrease [c] (if (= c 0) 6 (dec c)))

(defn solve1 [file]
  (loop [days 0 input (read-input file)]
    (if (>= days 80)
      (count input)
      (let [zeros (count (filter zero? input))
            input (map decrease input)]
        (if (zero? zeros)
          (recur (inc days) input)
          (recur (inc days) (concat input (repeat zeros 8))))))))
       

(println (solve1 "test-input")) ;; 5934
