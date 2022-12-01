(ns santa.2021.day4
  (:require [clojure.string :refer [join split split-lines]]))

;; read input lines
(defn input-lines [file] (split-lines (slurp file)))

(defn parse-line [line] (map read-string line))

(defn parse-board [board]
  (let [lines (map #(split % #" ") board)]
    (map parse-line (for [line lines] (filter #(not (= % "")) line)))))

;; parse board input
;; boards are separated by single blank line
(defn parse-boards [board-strings]
  (let [board-lines (partition 5 5 ((group-by (fn [line] (not (= line ""))) board-strings) true))]
    (map parse-board board-lines)))
        
;; parse input lines and return draw numbers and boards separately
(defn parse-input-lines [lines]
  {:draws (map read-string (split (first lines) #","))
   :boards (parse-boards (rest (rest lines)))})

;; mark draw number in a single board row
(defn mark-row [colls draw] (remove #(= % draw) colls))

;; return board with draw numbers removed
(defn mark-draw [draw] (fn [board] (mapv #(mark-row % draw) board)))

(defn transpose [matrix] (apply mapv vector matrix))

(defn winning-row? [row] (zero? (count row)))

(defn winning-board? [board]
  (let [r (map winning-row? board)
        c (map winning-row? (transpose board))
        w (count (remove false? (flatten [r c])))]
    (> w 0)))

(defn find-winner [boards]
  (loop [boards boards]
    (if-not (seq boards)
      nil
      (if (winning-board? (first boards))
        (first boards)
        (recur (rest boards))))))

(defn solve1 [file]
  (let [input (parse-input-lines (input-lines file))
        draws (:draws input)
        boards (:boards input)]
    (loop [draws draws boards boards]
      (if-not (seq draws)
        nil
        (let [draw (first draws)
              boards (map (mark-draw draw) boards)
              winner (find-winner boards)]
          (if (not (nil? winner))
            (* draw (apply + (flatten winner)))
            (recur (rest draws) boards)))))))

(println (solve1 "input"))
