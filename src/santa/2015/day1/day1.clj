(ns santa.2015.day1)

(defn count-floors [input]
  (loop [input input floors 0]
    (if-not (seq input)
      floors
      (if (= (first input) \()
        (recur (rest input) (inc floors))
        (recur (rest input) (dec floors))))))

;; solution to part 1
(println (count-floors (slurp "input-2015-1")))

(defn find-basement-pos [input]
  (loop [input input floors 0 pos 0]
    (if (= floors (- 1))
      pos
      (if-not (seq input)
        floors
        (if (= (first input) \()
          (recur (rest input) (inc floors) (inc pos))
          (recur (rest input) (dec floors) (inc pos)))))))

;; solution to part 2
(println (find-basement-pos (slurp "input-2015-1")))

