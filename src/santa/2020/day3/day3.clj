(ns santa.2020.day3)

(require '[clojure.string :refer [join]])

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

(defn is-tree? [c] (= c \#))

(defn input->vec [input] (map vec input))

(defn indices 
  [rows cols] 
  (into [] (map #(- % 1) (take rows (take-nth 3 (take (* rows rows) (flatten (repeat (range 1 (+ cols 1))))))))))

(defn count-trees [input]
  (let [idxs (indices (count input) (count (first input)))]
    (loop [rows (input->vec input) idxs idxs trees 0]
      (if-not (seq rows)
        trees
        (let [idx (first idxs) row (first rows)]
          (if (is-tree? (row idx))
            (recur (rest rows) (rest idxs) (+ trees 1))
            (recur (rest rows) (rest idxs) trees)))))))

(println (count-trees test-input-3-1))


