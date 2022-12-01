(ns santa.2021.day4
  (:require [clojure.string :refer [join split split-lines]]
            [clojure.pprint :refer [pprint]]))

;; read input lines
(defn input-lines [file] (split-lines (slurp file)))

(defn parse-board [board]
  (let [lines (map #(split % #" ") board)]
    (for [line lines] (filter #(not (= % "")) line))))

;; parse board input
;; boards are separated by single blank line
(defn parse-boards [board-strings]
  (let [board-lines (partition 5 5 ((group-by (fn [line] (not (= line ""))) board-strings) true))]
    (map parse-board board-lines)))
        
;; mark draw number in a single board row
(defn mark-row [colls draw] (replace {draw nil } colls))

;; return board with draw numbers replaced by mark
(defn mark-draw [draw] (fn [board] (mapv #(mark-row % draw) board)))

(defn mark-boards [draw boards] (map (mark-draw draw) boards))

(defn transpose [matrix] (apply mapv vector matrix))

(defn winning-row? [row] 
  (= (count row)
     (count (filter nil? row))))

(defn unempty? [coll] (not (empty? (remove false? coll))))

(defn winning-board? [board]
  (or (unempty? (filter winning-row? board))
      (unempty? (filter winning-row? (transpose board)))))

;; remove n items from collection starting from idx.
(defn splice-coll [coll idx n] (concat (take idx coll) (drop (+ idx n) coll)))

(defn find-winner [boards]
  (loop [pos 0 boards boards]
    (if-not (seq boards)
      nil
      (if (winning-board? (first boards))
        (do
          (println (first boards))
          {:pos pos :board (first boards)})
        (recur (inc pos) (rest boards))))))

(defn calc-sum [board draw]
  (let [nums (remove nil? (flatten board))
        sum (apply + (map read-string nums))
        res (* (read-string draw) sum)]
    res))

(defn solve2 [file]
  (let [input (input-lines file)
        draws (split (first input) #",")
        boards (parse-boards (rest (rest input)))]
    (loop [draws draws boards boards sums []]
      (if-not (seq boards)
        sums
        (if-not (seq draws)
          sums
          (let [draw (first draws)
                boards (mark-boards draw boards)
                winner (find-winner boards)]
            (if (not (nil? winner))
              (recur 
                (rest draws)
                (splice-coll boards (:pos winner) 1)
                (calc-sum (:board winner) draw))
              (recur (rest draws) boards sums))))))))

(pprint (solve2 "input")) ;; 1924
