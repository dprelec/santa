(ns santa.2020.day3)

(defn is-tree? [c] (= c \#))

(defn get-indices [w h] (take h (map #(mod % w) (map #(+ 1 (* 3 %)) (range)))))

(def test-input-3-1
  [
"..##......."
"#...#...#.."
".#....#..#."
"..#.#...#.#"
".#...##..#."
"..#.##....."
".#.#.#....#"
".#........#"
"#.##...#..."
"#...##....#"
".#..#...#.#"
])

(defn count-trees [input]
  (let [indices (get-indices (count (first input)) (count input))
        input (flatten (map vec (repeat input)))]
    (loop [input input indices indices trees 0]
      (if-not (seq indices)
        trees
        (if (is-tree? ((vec (first input)) (first indices)))
          (recur (rest input) (rest indices) (+ 1 trees))
          (recur (rest input) (rest indices) trees))))))

(defn enumerate-trees [input]
  (let [indices (get-indices (count (first input)) (count input))]
    (println indices)
    (loop [input input indices indices trees 0]
      (println trees)
      (if-not (seq indices)
        trees
        (if (is-tree? ((vec (first input)) (first indices)))
          (recur (rest input) (rest indices) (+ 1 trees))
          (recur (rest input) (rest indices) trees))))))
