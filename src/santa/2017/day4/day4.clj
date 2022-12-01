(ns day4 (:require [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; no-doubles? returns true if the number of duplicate entries in
;; frequency map is zero, and false otherwise.
(defn no-doubles? [freqs] (empty? (for [kv freqs :when (> (val kv) 1)] kv)))

(defn parse-row [row] (split row #" "))

(defn solve-1 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; parse passwords list
        passwords (map parse-row input)
        ;; calculate password frequencies
        freqs (map frequencies passwords)
        ;; filter out rows with duplicate passwords
        matches (filter no-doubles? freqs)]
    (count matches)))

(println "Test solution 1:" (solve-1 "test-input"))
(println "Solution 1:" (solve-1 "input"))

(defn anagram? [word1 word2] (= (sort word1) (sort word2)))

(defn no-anagrams? [row]
  (loop [anagrams [] row row]
    ;; don't compare last item with itself
    (if (= (count row) 1) 
      (empty? anagrams)
      ;; compare first item with all the rest
      ;; continue for all the rest items recursively
      (let [x (first row)
            xs (rest row)
            af (fn [c] (anagram? x c))
            matches (filter af xs)]
        (if (empty? matches)
          (recur anagrams xs)
          (recur (conj anagrams x) xs))))))

(defn solve-2 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; parse passwords list
        passwords (map parse-row input)
        ;; filter out rows with anagrams
        matches (filter no-anagrams? passwords)]
    (count matches)))

(println "Test solution 2:" (solve-2 "test-input-2")) ;; expect 3
(println "Solution 2:" (solve-2 "input")) 

