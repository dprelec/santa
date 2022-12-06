(ns day6 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-line [line w]
  (let [;; partition into windows of w chars
        part (partition w 1 (split line #""))
        ;; indexed char frequencies: [0 {freqs} 1 {freqs} ...]
        freqs (map-indexed vector (map frequencies part))
        ;; take first match where all freq values are eq to 1
        same (first (filter #(apply = 1 (vals (second %))) freqs))
        idx (first same)]
    (+ w idx)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        line (first input)]
    (solve-line line 4)))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)
        line (first input)]
    (solve-line line 14)))

(println "Solution 1:" (solve-1 "input"))
(println "Solution 2:" (solve-2 "input"))
