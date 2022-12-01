(ns santa.2021.day4
  (:require [clojure.string :refer [join split split-lines]]))

;; read input lines
(defn input-lines [file] (split-lines (slurp file)))

(defn parse-line [line] (map read-string line))

(defn parse-board [board]
  (let [lines (map #(split % #" ") board)]
    (map parse-line (for [line lines] (filter #(not= % "") line)))))

;; parse board input
;; boards are separated by single blank line
(defn parse-boards [board-strings]
  (let [board-lines (partition 5 5 ((group-by #(not= % "") board-strings) true))]
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

(defn splice-coll [coll n c] (concat (take n coll) (drop (+ n c) coll)))

(defn find-winner [boards]
  (loop [idx 0 boards boards]
    (if-not (seq boards)
      nil
      (if (winning-board? (first boards))
        {:idx idx :board (first boards)}
        (recur (inc idx) (rest boards))))))

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
          (println :board-count (count boards))
          (if (= 1 (count boards))
            (* draw (apply + (flatten boards)))
            (if (not (nil? winner))
              (do
                (println :idx (:idx winner) 
                         :winner (:board winner) 
                         :sum (* draw (apply + (flatten (:board winner)))))
                (recur (rest draws) (splice-coll boards (:idx winner) 1)))
              (recur (rest draws) boards))))))))

(println (solve1 "test-input"))

;(println (solve1 "-input"))
