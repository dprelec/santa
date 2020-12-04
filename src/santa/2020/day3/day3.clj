(ns santa.2020.day3)

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

(defn char-is-tree? [c] (= c \#))

(defn indices 
  [rows cols step] 
  (into [] (map dec (take rows (take-nth step (take (* rows rows) (cycle (range 1 (inc cols)))))))))

(defn count-trees [input step-right step-down]
  (loop [rows (take-nth step-down input)
         idxs (indices (count input) (count (first input)) step-right)
         trees 0]
      (if-not (seq rows)
        trees
        (let [idx (first idxs) row (first rows)]
          (if (char-is-tree? ((vec row) idx))
            (recur (rest rows) (rest idxs) (inc trees))
            (recur (rest rows) (rest idxs) trees))))))

(println 
  (time (apply * [(count-trees test-input-3-1 3 1)
                  (count-trees test-input-3-1 1 1)
                  (count-trees test-input-3-1 5 1)
                  (count-trees test-input-3-1 7 1)
                  (count-trees test-input-3-1 1 2)])))
