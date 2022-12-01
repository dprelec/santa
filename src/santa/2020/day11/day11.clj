(ns santa.2020.day11
  (:require [clojure.string :refer [split-lines split]]))

(def floor \.)
(def empty-seat \L)
(def occupied-seat \#)

(defn floor? [c] (= c floor))
(defn seat-empty? [c] (= c empty-seat))
(defn seat-occupied? [c] (= c occupied-seat))

(defn rows [a] (alength a))
(defn cols [a] (alength (first a)))

(defn outside? [[x y] r c]
  (or (< x 0) (< y 0) (>= x r) (>= y c)))

(defn neighbours [a r c]
  (let [mr (rows a) 
        mc (cols a)
        r-1 (dec r)
        r+1 (inc r)
        c-1 (dec c)
        c+1 (inc c)]
    (remove #(outside? % mr mc)
            [[r-1 c-1] [r-1 c] [r-1 c+1]
             [r   c-1]         [r   c+1]
             [r+1 c-1] [r+1 c] [r+1 c+1]])))

(defn count-neighbours [a i j]
  (let [nb (neighbours a i j)
        co (map #(aget a (first %) (second %)) nb)
        oc (filter seat-occupied? co)
        c (count oc)]
    c))

(defn occupy-seat [a b i j]
  (aset b i j (aget a i j))
  (when (not (floor? (aget a i j)))
    (let [n (count-neighbours a i j)]
      (if (and (seat-empty? (aget a i j)) (zero? n))
        (aset b i j occupied-seat)
        (if (and (seat-occupied? (aget a i j)) (>= n 4))
          (aset b i j empty-seat))))))

(defn occupy [a]
  (let [b (make-array Character/TYPE (rows a) (cols a))
        c (for [i (range (rows a)) j (range (cols a))] [i j])]
    (loop [c c]
      (when (seq c)
        (let [[i j] (first c)]
          (occupy-seat a b i j)
          (recur (rest c)))))
    b))

(defn differences [a b]
  (remove nil? (for [i (range (rows a)) j (range (cols a))]
                   (when (not= (aget a i j) (aget b i j))
                     {:i i :j j}))))

(defn count-seats [a]
  (count (filter seat-occupied? (for [i (range (rows a)) j (range (cols a))] (aget a i j)))))

(defn input->array [input-file] (to-array-2d (split-lines (slurp input-file))))

(defn iterate-occupy [a]
  (loop [a a d 0]
    (let [b (occupy a)
          e (count (differences a b))]
      (if (= d e)
        b
        (recur b e)))))

;; solution to part 1
(println (count-seats (iterate-occupy (input->array "test-input"))))
(println (count-seats (iterate-occupy (input->array "input"))))

