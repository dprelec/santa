(ns santa.2021.day4
  (:require [clojure.string :refer [join split split-lines]]
            [clojure.pprint :refer [pprint]]))

(defn parse-boards [lines]
  (let [lines (partition 5 5 ((group-by #(not= % "") lines) true))
        lines (filter #(not= % "") (map #(split % #" ") lines))]
    lines))

(defn read-input [file]
  (let [lines (split-lines (slurp file))
        draws (first lines)
        draws (split draws #",")
        boards (rest (rest lines))
        boards (parse-boards boards)]
    (println :draws draws)
    (println :boards boards)))

(read-input "test-input")
