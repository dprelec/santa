(ns santa.2020.day6
    (:require [clojure.string :refer [split split-lines]]))

(defn input->lines [input-file]
  (filter 
    ;; skip lines with one space only
    #(not= % '(""))
    ;; separate entries by one space
    (partition-by #(= % "") (split-lines (slurp input-file)))))

(defn strings->set 
  "Convert list of strings (ab bc) into set (a b c)."
  [strings]
  (let [s (for [s strings] (split s #""))]
    (set (apply concat (set s)))))
  
(defn count-sums [lines]
  (apply + (for [l lines]
    (let [s (strings->set l)]
      (count s)))))

;; solution to part 1
(println (count-sums (input->lines "input")))

(defn strings->map [strings]
  (loop [strings strings m {}]
    (if-not (seq strings)
      m
      (recur (rest strings) (merge-with + m (frequencies (split (first strings) #"")))))))

(defn count-answers [group]
  (let [m (strings->map group) s (count group)]
    (count (filter #(= (val %) s) m))))

(defn total-answers [lines]
  (apply + (for [l lines] (count-answers l))))
             
(defn total-answers-d [lines]
  (for [l lines] (count-answers l)))

;; solution to part 2
(println (total-answers (input->lines "input")))
