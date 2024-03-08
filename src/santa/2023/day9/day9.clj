(ns day9 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn enumerate-histories [numbers]
  (loop [histories [numbers]]
    (let [history (map (fn [[a b]] (- b a)) (partition 2 1 (last histories)))]
      (if (every? zero? history)
        (conj histories history)
        (recur (conj histories history))))))

(defn extrapolate-last [histories]
    (reduce (fn [a b] (+ a (last (second b)))) 
            0 
            (partition 2 1 (reverse histories))))

(defn parse-line [line]
  (let [line (split line #"\s+")]
    (mapv read-string line))) 

(defn solve-1 [input-file]
  (let [input (input->vec input-file)]
    (apply + 
           (->> (map parse-line input)
                (map enumerate-histories)
                (map extrapolate-last)))))

(defn extrapolate-first [histories]
    (reduce (fn [a b] (- (first (second b)) a)) 
            0 
            (partition 2 1 (reverse histories))))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)]
    (apply + 
           (->> (map parse-line input)
                (map enumerate-histories)
                (map extrapolate-first)))))

(println {:part-1-test (solve-1 "test-input")})
(println {:part-1-solution (solve-1 "input")})

(println {:part-2-test (solve-2 "test-input")})
(println {:part-2-solution (solve-2 "input")})

