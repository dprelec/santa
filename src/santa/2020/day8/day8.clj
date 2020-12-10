(ns santa.2020.day8
 (:require [clojure.string :refer [split-lines split]]))

(defn input->program
  "Convert input lines into map of instructions like:
  {0 [nop +0], 1 [acc +1], 2 [jmp +4] ..}."
  [input-file]
  (let [lines (split-lines (slurp input-file))]
    (loop [lines lines acc [] i 0]
      (if-not (seq lines)
        acc
        (let [line (first lines) part (split line #" ")]
          (recur (rest lines) (conj acc part) (inc i)))))))

(defn execute-program 
  "Executes a map of instructions. Stops when pointer value repeats for the
  second time."
  [prog]
  (loop [ptr 0 acc 0 seen #{}]
    (let [instr (nth prog ptr)]
      (if (or (nil? instr) (contains? seen ptr))
        acc
        (let [[op i] instr nxt (read-string i)]
          (case op
            "nop" (recur (inc ptr) acc (conj seen ptr))
            "acc" (recur (inc ptr) (+ acc nxt) (conj seen ptr))
            "jmp" (recur (+ ptr nxt) acc (conj seen ptr))))))))

;; solution to part 1
(println (execute-program (input->program "test-input")))
(println (execute-program (input->program "input")))

