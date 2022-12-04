(ns day10 (:require [clojure.string :refer [split]]))

(def input (map read-string (split "1113122113" #"")))

(defn solve-1 [input n]
  (loop [input input c n]
    (let [;; partition by identity grouping the same numbers 
          ;; together
          parts (partition-by identity input)
          ;; count number of elements in each sumlist
          counts (map count parts)
          ;; enumeate all elements
          elems (map first parts)
          ;; create new input with zip
          input (flatten (map vector counts elems))]
      (if (= 1 c)
        (count input)
        (recur input (dec c))))))

(println (solve-1 input 40))
(println (solve-1 input 50))
