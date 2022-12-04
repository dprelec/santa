(ns day3)

(defn solve-1 [input]
  (loop [input input x 0 y 0 grid {}]
    (if-not (seq input)
      (count grid)
      (case (first input)
        \> (recur (rest input) (inc x) y (assoc grid [(inc x) y] 1))
        \< (recur (rest input) (dec x) y (assoc grid [(dec x) y] 1))
        \^ (recur (rest input) x (inc y) (assoc grid [x (inc y)] 1))
        \v (recur (rest input) x (dec y) (assoc grid [x (dec y)] 1))))))

;; solution to part 1
(println (solve-1 (slurp "input")))

(defn solve-2 [input]
  (loop [input input x 0 y 0 santa-grid {} robo-grid {}]
    (if-not (seq input)
      (+ (count santa-grid) (count robo-grid))
      (case (first input)
        \> (recur (rest input) (inc x) y (assoc grid [(inc x) y] 1))
        \< (recur (rest input) (dec x) y (assoc grid [(dec x) y] 1))
        \^ (recur (rest input) x (inc y) (assoc grid [x (inc y)] 1))
        \v (recur (rest input) x (dec y) (assoc grid [x (dec y)] 1))))))
