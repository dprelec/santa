(ns day4 (:require [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-1 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; range extractor
        parse (fn [row] (re-seq #"\d+" row))
        ;; parse ranges
        ranges (mapv #(map read-string (parse %)) input)]
    (count
     (filter
      (fn [row]
        (let [[x1 y1 x2 y2] row]
          (or (<= x1 x2 y2 y1)
              (<= x2 x1 y1 y2))))
      ranges))))

(println (solve-1 "test-input"))
(println (solve-1 "input"))

(defn solve-2 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; range extractor
        parse (fn [row] (re-seq #"\d+" row))
        ;; parse ranges
        ranges (mapv #(map read-string (parse %)) input)]
    (count
     (filter
      (fn [row]
        (let [[x1 y1 x2 y2] row]
          (or (<= x1 x2 y2 y1)
              (<= x2 x1 y1 y2)
              (<= x1 x2 y1 y2)
              (<= x2 x1 y2 y1))))
      ranges))))

(println (solve-2 "test-input"))
(println (solve-2 "input"))
