(ns day2 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(def shapes
  {"A" "rock" "X" "rock"
   "B" "paper" "Y" "paper"
   "C" "scissors" "Z" "scissors"})

(def scores
  {"rock" 1
   "paper" 2
   "scissors" 3})

(def draws
  {"Y" "draw"
   "X" "lost"
   "Z" "win"})

(def outcome-scores
  {"lost" 0
   "draw" 3
   "win" 6})

;; Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
(defn outcome [opponent you]
  (let [opponent-shape (shapes opponent)
        you-shape (shapes you)
        outcome (case [you-shape opponent-shape]
                  ["rock" "scissors"] 6
                  ["rock" "paper"] 0
                  ["scissors" "paper"] 6
                  ["scissors" "rock"] 0
                  ["paper" "rock"] 6
                  ["paper" "scissors"] 0
                  ["rock" "rock"] 3
                  ["paper" "paper"] 3
                  ["scissors" "scissors"] 3)]
    (+ (scores you-shape) outcome)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        input (map #(split % #" ") input)
        scores (map #(outcome (first %) (second %)) input)]
    (apply + scores)))

(println (solve-1 "test-input"))
(println (solve-1 "input"))

;; Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
(defn strategy [opponent you]
  (let [opponent-shape (shapes opponent)
        you-shape (draws you)
        outcome (case [opponent-shape you-shape]
                  ["rock" "draw"] (scores "rock")
                  ["rock" "win"] (scores "paper")
                  ["rock" "lost"] (scores "scissors")
                  ["scissors" "draw"] (scores "scissors")
                  ["scissors" "win"] (scores "rock")
                  ["scissors" "lost"] (scores "paper")
                  ["paper" "draw"] (scores "paper")
                  ["paper" "win"] (scores "scissors")
                  ["paper" "lost"] (scores "rock"))
        score (+ outcome (outcome-scores you-shape))]
    score))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)
        input (map #(split % #" ") input)
        scores (map #(strategy (first %) (second %)) input)]
    (apply + scores)))

(println (solve-2 "test-input"))
(println (solve-2 "input"))
