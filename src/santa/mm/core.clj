(ns mm.core)

(defn =2020 [n] (= (:sum n) 2020))

(defn day-1-1 [input] 
  (apply * (first (into #{} (map :set (filter =2020 (for [x input y input] {:set (set [x y]) :sum (+ x y)})))))))

(defn day-1-2 [input]
  (apply * (first (into #{} (map :set (filter =2020 (for [x input y input z input] {:set (set [x y z]) :sum (+ x y z)}))))))

