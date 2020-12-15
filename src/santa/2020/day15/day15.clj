(ns santa.2020.day15
 (:require [clojure.string :refer [split-lines split]]))

(defn parse-input [input] (into [] (map read-string (split input #","))))

(defn init-idx [input]
  (into {} (map (fn [a] {(second a) (first a)}) (map-indexed vector input))))

(defn rev-rest [l] (reverse (rest (reverse l))))

(defn find-last [input n]
  (loop [lst (last input) prev 0 seen (init-idx (rev-rest input)) i (dec (count input))]
    (if (= i n)
      prev
      (let [nxt (if (seen lst) (- i (seen lst)) 0)]
        (recur nxt lst (assoc seen lst i) (inc i))))))

(println (find-last (parse-input "9,6,0,10,18,2,1") 2020))
(println (find-last (parse-input "9,6,0,10,18,2,1") 30000000))



