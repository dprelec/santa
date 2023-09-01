(ns day18 (:require [clojure.set :refer [intersection]]
                    [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn count= [n] (fn [c] (= (count c) n)))

;; cube-points returns set of all cube points
(defn cube-points [x0 y0 z0]
  (set (for [x [x0 (inc x0)]
             y [y0 (inc y0)]
             z [z0 (inc z0)]]
         [x y z])))

;; cube-points returns set of all cube points
(defn cube-points-vec [x0 y0 z0]
  (vec (for [x [x0 (inc x0)]
             y [y0 (inc y0)]
             z [z0 (inc z0)]]
         [x y z])))

(defn solve1 [input-file]
  (let [input (input->vec input-file)
        input (map #(map read-string (split % #",")) input)
        ;; total number of added sides
        sides (* 6 (count input))]
    (loop [input input cubes [] extras 0]
      (if-not (seq input)
        (- sides (* 2 extras))
        (let [[x y z] (first input)
              cube (cube-points x y z)
              common (filter (partial (count= 4)) (map (partial intersection cube) cubes))]
          (recur (rest input) (conj cubes cube) (+ extras (count (set common)))))))))

(defn cube-planes [x y z]
  (let [points (cube-points-vec x y z)]
    [(for [idx [0 1 2 3]] (nth points idx))
     (for [idx [4 5 6 7]] (nth points idx))
     (for [idx [0 1 5 4]] (nth points idx))
     (for [idx [1 2 6 5]] (nth points idx))
     (for [idx [2 3 7 6]] (nth points idx))
     (for [idx [0 3 7 4]] (nth points idx))]))

(defn solve1-with-map [input-file]
  (let [input (input->vec input-file)
        input (map #(map read-string (split % #",")) input)]
    (loop [input input planes [] cubes []]
      (if-not (seq input)
        (let [freqs (frequencies planes)
              cf (for [cube cubes] (for [plane cube] (freqs plane)))]
          {:planes (count planes)
           :cubes (count cubes)
           :freqs (count freqs)
           :cf cf
           :cands (count (filter #(> (val %) 1) freqs))
           :fvals (frequencies (for [f freqs] (val f)))})
        (let [[x y z] (first input)
              cube (cube-planes x y z)]
          (recur (rest input) (concat planes cube) (conj cubes cube)))))))

;(println "Test 1:" (solve1 "test-input-2"))
;(println "Test 1.2:" (solve1 "test-input"))
;(println "Solution 1:" (solve1 "input"))

;(println "Test 1:" (solve1-with-map "test-input-2"))
;(println "Test 1.2:" (solve1-with-map "test-input"))
;(println "Solution 1:" (solve1-with-map "input"))

(defn solve2 [input-file]
  (let [input (input->vec input-file)
        input (map #(map read-string (split % #",")) input)
        f_z (frequencies (for [row input] [(nth row 0) (nth row 1)]))
        f_x (frequencies (for [row input] [(nth row 1) (nth row 2)]))
        f_y (frequencies (for [row input] [(nth row 0) (nth row 2)]))
        f_z2 (frequencies (for [row input] [(inc (nth row 0)) (inc (nth row 1))]))
        f_x2 (frequencies (for [row input] [(inc (nth row 1)) (inc (nth row 2))]))
        f_y2 (frequencies (for [row input] [(inc (nth row 0)) (inc (nth row 2))]))
        s1 (solve1 input-file)
        ext (+ (count (vals f_x)) (count (vals f_y)) (count (vals f_z))
               (count (vals f_x2)) (count (vals f_y2)) (count (vals f_z2)))]
    [s1 ext]))


(defn solve2-old [input-file]
  (let [input (input->vec input-file)
        input (map #(map read-string (split % #",")) input)
        all_x (for [row input] (nth row 0))
        all_y (for [row input] (nth row 1))
        all_z (for [row input] (nth row 2))
        x (count (range (inc (apply max all_x)) (apply min all_x) -1))
        y (count (range (inc (apply max all_y)) (apply min all_y) -1))
        z (count (range (inc (apply max all_z)) (apply min all_z) -1))
        v (* x y z)
        c (count input)]
    {:x x :y y :z z :v v :c c :d (- v c)}))

(println "Test 1:" (solve2 "test-input-2"))
(println "Test 1.2:" (solve2 "test-input"))
(println "Solution 1:" (solve2 "input"))

;(solve2 "input")
