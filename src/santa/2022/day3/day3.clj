(ns day3 (:require [clojure.set :refer [intersection]]
                   [clojure.string :refer [split-lines]]))

(def to-letter (comp str char))
(def lowercase-letters (map to-letter (range 97 123)))
(def uppercase-letters (map to-letter (range 65 91)))

;; map of letter priorities: {"a" 1, "b" 2, ...}
(def priorities
  (merge
   (zipmap lowercase-letters (range 1 27))
   (zipmap uppercase-letters (range 27 53))))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn line-priority [line]
  (let [;; split line in two parts
        parts (split-at (/ (count line) 2) line)
        ;; convert parts to sets
        parts (map set parts)
        ;; find common item
        common (apply intersection parts)
        ;; extract member
        common (str (first common))]
    (priorities common)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        priorities (map line-priority input)]
    (apply + priorities)))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)
        groups (partition 3 3 input)
        ;; apply intersection to each group of sets
        commons (map #(apply intersection (map set %)) groups)
        ;; extract all common members
        commons (map #(str (first %)) commons)
        priorities (map priorities commons)]
    (apply + priorities)))

(println "    Test 1:" (solve-1 "test-input"))
(println "Solution 1:" (solve-1 "input"))

(println "    Test 2:" (solve-2 "test-input"))
(println "Solution 2:" (solve-2 "input"))
