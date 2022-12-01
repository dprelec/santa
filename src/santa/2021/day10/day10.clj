(ns santa.2021.day10
  (:require [clojure.string :refer [join split split-lines]]))

(defn read-input [file] (split-lines (slurp file)))

(def bracemap 
  {\{ \}
   \[ \]
   \( \]
   \< \>})
         
(def braces "[({(<(())[]>[[{[]{<()<>>")

(println (solve1 "test-input"))
