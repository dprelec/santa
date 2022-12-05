(ns day5 (:require [clojure.string :refer [join split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn transpose [matrix] (apply mapv vector matrix))

(defn crate? [line]
  (let [line (remove #(= \[ %) line)
        line (remove #(= \] %) line)
        line (remove #(= \space %) line)]
    (> (count line) 0)))

(defn space? [c] (= c \space))

(defn top [crates]
  (for [k (sort (keys crates))]
    (last (crates k))))

(defn solution-part-1 [crates moves]
  (loop [moves moves crates crates]
    (if-not (seq moves)
      (join "" (top crates))
      (let [;; parse move lines
            move (split (first moves) #" ")
            ;; amount is integer
            amount (read-string (nth move 1))
            ;; from and to are string keys of crates
            from (nth move 3)
            to (nth move 5)
            payload (split-at (- (count (crates from)) amount) (crates from))
            ;; source crates to be moved
            new-crate (concat (crates to) (reverse (first (rest payload))))
            old-crate (first payload)
            ;; modify crates 
            crates (assoc crates to new-crate)
            crates (assoc crates from old-crate)]
        (recur (rest moves) crates)))))

(defn solution-part-2 [crates moves]
  (loop [moves moves crates crates]
    (if-not (seq moves)
      (join "" (top crates))
      (let [;; parse move lines
            move (split (first moves) #" ")
            ;; amount is integer
            amount (read-string (nth move 1))
            ;; from and to are string keys of crates
            from (nth move 3)
            to (nth move 5)
            ;; source crates to be moved
            payload (split-at (- (count (crates from)) amount) (crates from))
            new-crate (concat (crates to) (first (rest payload)))
            old-crate (first payload)
            ;; modify crates 
            crates (assoc crates to new-crate)
            crates (assoc crates from old-crate)]
        (recur (rest moves) crates)))))

(defn parse-crate-moves [input]
  (let [;; split config in two parts
        config (partition-by #(= "" %) input)
        ;; rotate crates config
        crates (map reverse (transpose (first config)))
        ;; remove non-crate lines
        crates (filter crate? crates)
        ;; clean-up crate lines
        crates (map #(remove space? %) crates)
        ;; convert to map {"crate_numer" [stack]}
        crates (into {} (for [crate crates] {(str (first crate)) (rest crate)}))
        moves (first (rest (rest config)))]
    [crates moves]))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        config (parse-crate-moves input)
        crates (first config)
        moves (second config)]
    (solution-part-1 crates moves)))

(println "T1:" (solve-1 "test-input"))
(println "P1:" (solve-1 "input"))

(defn solve-2 [input-file]
  (let [input (input->vec input-file)
        config (parse-crate-moves input)
        crates (first config)
        moves (second config)]
    (solution-part-2 crates moves)))

(println "T2:" (solve-2 "test-input"))
(println "P2:" (solve-2 "input"))
