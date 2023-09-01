(ns day22 (:require [clojure.string :refer [join split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; set of valid rotations
(def rotations #{"L" "R"})

;; convert-path-element converts path elements:
;; - rotations are returned as is
;; - steps are parsed as integers
(defn convert-path-element [element]
  (if (rotations element)
    element
    (read-string element)))

;; parse-path parses direction line like "10R5L5R10L4R5L5"
;; into sequence of steps and rotations 
;; (10 "R" 5 "L" 5 "R" 10 "L" 4 "R" 5 "L" 5)
(defn parse-path [line]
  (let [;; split into separate chars
        parts (split line #"")
        ;; partition by rotations-set membership check
        partitioned (partition-by rotations parts)
        ;; join parts of each sublist together
        parsed (map (partial join "") partitioned)
        ;; parse numbers, leave rotations as is
        parsed (map convert-path-element parsed)]
    parsed))

;; get-starting-pos returns position of first non-empty-space element in a
;; row.
(defn get-starting-pos [row]
  (let [;; add indices
        indexed (map-indexed vector row)
        ;; remove empty-space elements
        space? #(= (second %) " ")
        candidates (remove space? indexed)
        x (-> candidates first first)]
    {:x x :y 0}))

;; XXX
(defn next-pos [grid pos direction]
  (case direction
    ">" (let [x (inc (:x pos))
              elem (:x (nth grid (:y pos)))]
          (case elem
            "." {:x x :y (:y pos)}
            "#" pos
            " " (get-starting-pos (nth grid (:y pos)))))))

(defn next-dir [dir rotation]
  (case [dir rotation]
    [">" "R"] "v"
    [">" "L"] "^"
    ["v" "R"] "<"
    ["v" "L"] ">"
    ["<" "R"] "^"
    ["<" "L"] "v"))

(defn solve1 [input-file]
  (let [input (input->vec input-file)
        ;; last line contains rotations
        path (parse-path (last input))
        ;; split grid into chars
        input (map #(split % #"") input)
        ;; remove last two lines 
        grid (->> input vec pop pop)
        ;; calculate dimensions
        rows (count grid)
        cols (apply max (map count grid))
        pos (get-starting-pos (first grid))
        dir ">"]
    {:grid grid :r rows :c cols :d path :pos pos :dir dir}))

(solve1 "test-input")


