(ns day9 (:require 
           [clojure.set :refer [union]]
           [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; parse input converts row like "R 2" into ["R" 2]
(defn parse-input [row]
  (let [parts (split row #" ")]
    [(first parts) (read-string (second parts))]))

(defn separated? [[hx hy] [tx ty]]
  (or (> (Math/abs (- hx tx)) 1)
      (> (Math/abs (- hy ty)) 1)))

(defn move-all [head tail dir amount]
  (loop [amount amount visited #{}]
    (if (zero? amount)
      {:head head :tail tail :visited visited}
      (let [head (map + head dir)
            tail (if (separated? head tail)
                   
          
;; move moves head and tail via op directions.
;; It returns hashmap containing:
;; {:visited set-of-tails-locations
;;  :head new-head
;;  :tail new-tail}
(defn move [head tail op]
  (let [direction (first op)
        amount (second op)]
    (case direction
      "L" (move-all head tail [0 -1] amount)
      "R" (move-all head tail [0 1] amount)
      "U" (move-all head tail [1 0] amount)
      "D" (move-all head tail [0 1] amount))))

(defn solve1 [input-file]
  (let [input (input->vec input-file)
        input (map parse-input input)]
    (loop [directions input visited #{} head [0 0] tail [0 0]]
      (if-not (seq directions)
        [(count visited) visited]
        (let [op (first directions)
              moved (move head tail op)
              visited (union (:visited moved))]
          (recur (rest directions) visited (:head moved) (:tail moved)))))))

(println (solve1 "test-input"))
