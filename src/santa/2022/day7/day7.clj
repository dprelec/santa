(ns day7 (:require [clojure.string :refer [join split split-lines starts-with?]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(def re-cd #"^\$ cd (.*)")
(def re-file #"^(\d+) (.*)")
(def re-dir #"^dir (.*)")
(def re-ls #"^\$ ls")

(defn same-level [root child]
  (let [rc (count (re-seq #"\/" root))
        cc (count (re-seq #"\/" child))]
    (println {:root root :child child :rc rc :cc cc})
    (= 1 (- cc rc))))

(defn print-tree [contents]
  (println {:contents contents})
  (let [paths (sort (set (cons "/" (keys contents))))]
    (println {:keys paths})
    (doseq [k paths]
      (let [child? (fn [dir]
                     (let [c (count k)
                           e (take (inc c) dir)
                           r (join "|" k)
                           p (join "|" e)]
                       (and
                        (not= p r)
                        (same-level r p)
                        (starts-with? p r))))
            children (filter child? paths)]
        ;(println k)))))
        (println {:key k :val (get contents k) :children children})))))


(defn solve-1 [input-file]
  (let [input (input->vec input-file)]
    (loop [;; original input
           input input
           ;; path elements [/ a b c ...]
           dirs []
           ;; contents mapped by path { "/a/b/c" [ ["dir" "a"] [1234 a.txt] ]
           contents {}]
      (if-not (seq input)
        (print-tree contents)
        (let [row (first input)
              parts (split row #" ")]
          (condp (comp seq re-seq) row
            re-cd (if (= ".." (nth parts 2))
                    (recur (rest input) (pop dirs) contents)
                    (recur (rest input) (conj dirs (nth parts 2)) contents))
            re-dir (recur (rest input) dirs contents)
            re-file (let [path (join "/" dirs)
                          old (contents path)
                          size (read-string (first parts))]
                      (if-not (nil? old)
                        (recur (rest input) dirs (assoc contents path (+ old size)))
                        (recur (rest input) dirs (assoc contents path size))))
            re-ls (recur (rest input) dirs contents)))))))

(solve-1 "test-input")

;; too low: 1624690
(solve-1 "input")
