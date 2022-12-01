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
        
(def mark "X")

(defn mark? [s] (= mark s))

;; mark draw number in a single board row
(defn mark-row [colls draw] (replace {draw mark } colls))

;; return board with draw numbers replaced by mark
(defn mark-draw [draw] (fn [board] (mapv #(mark-row % draw) board)))

(defn transpose [matrix] (apply mapv vector matrix))

(defn winning-row? [row] 
  (= (count row)
     (count (filter #(= % mark) row))))

(defn unempty? [coll] (not (empty? (remove false? coll))))

(defn winning-board? [board]
  (or (unempty? (map winning-row? board))
      (unempty? (map winning-row? (transpose board)))))

(defn find-winner [boards]
  (loop [pos 0 boards boards]
    (if-not (seq boards)
      nil
      (if (winning-board? (first boards))
        (do 
          (pprint (first boards))
          {:pos pos :board (first boards)})
        (recur (inc pos) (rest boards))))))

(defn winner? [board] (not (nil? board)))

;; remove n items from collection starting from idx.
(defn splice-coll [coll idx n] (concat (take idx coll) (drop (+ idx n) coll)))

(defn solve1 [file]
  (let [lines (input-lines file)
        draws (split (first lines) #",")
        boards (parse-boards (rest (rest lines)))]
    (loop [draws draws boards boards]
      (if-not (seq draws)
        nil
        (let [draw (first draws)
              boards (map (mark-draw draw) boards)
              winner (find-winner boards)]
          (if (winner? winner)
            (let [win (flatten (:board winner))
                  win (remove mark? win)
                  nums (map read-string win)]
              (* (read-string draw) (apply + nums)))
            (recur (rest draws) boards)))))))

;(println :test-solution-1 (solve1 "test-input")) ;; 4512
;(println :real-solution-1 (solve1 "input"))      ;; 25410

(defn sum-winner [board draw]
  (let [win (flatten board)
        win (remove mark? win)
        nums (map read-string win)
        sum (apply + nums)]
    (* (read-string draw) sum)))

(defn solve2 [file]
  (let [lines (input-lines file)
        draws (split (first lines) #",")
        boards (parse-boards (rest (rest lines)))]
    (loop [draws draws boards boards sums []]
      (println :count (count boards) :draw (first draws) :rest-draws (count draws))
      (if-not (seq draws)
         sums
        (let [draw (first draws)
              boards (map (mark-draw draw) boards)
              winner (find-winner boards)]
          (if (winner? winner) 
            (recur 
              (rest draws) 
              (splice-coll boards (:pos winner) 1)
              (conj sums (sum-winner (:board winner) draw)))
            (recur (rest draws) boards sums)))))))

;(println :test-solution-2 (solve2 "test-input")) ;; 1924
(println :real-solution-2 (solve2 "input"))
