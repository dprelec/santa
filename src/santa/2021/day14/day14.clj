(ns santa.2021.day14
  (:require [clojure.string :refer [join split split-lines]]))

(defn read-input [file] 
  (map read-string (split (slurp file) #",")))

;; convert template like "NNCB" into list of pairs:
;; ("NN" "NC" "CB")
(defn template->pairs [template]
  (map join (partition 2 1 template)))

;; transform template like "NNCB" with rule ["NN" "C"]
;; into "NCNCB"
(defn transform-template [template rule]
  (if (= template (rule 0))
    (join 


