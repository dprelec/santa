(ns day5 (:require [clojure.set :refer [intersection]]
                   [clojure.string :refer [split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn is-vowel? [c] (contains? #{\a \e \i \o \u} c))

(defn nice-string [row]
  (if (.contains row "xy")
    false
    (if-let [vowel-count (count-vowels row)]
      (when (zero? vowel-count)
        false))))

(defn nice-strings [rows]
  (count (filter nice-string rows)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        nice-count (count (nice-strings input))]
    nice-count))

        
