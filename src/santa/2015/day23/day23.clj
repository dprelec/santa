(ns day23 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; valid registers - no parsing :)
(def register
  {"a," "a"
   "b," "b"
   "a" "a"
   "b" "b"})

(defn solve-1 [input-file a b]
  (let [input (input->vec input-file)
        registers {"a" a "b" b}]
    (loop [pos 0 registers registers]
      (if (>= pos (count input))
        registers
        (let [;; read current instruction row
              instr (input pos)
              ;; split into op and args
              instr (split instr #" ")
              ;; instruction operation
              op (first instr)
              ;; arguments: [register, jump location]
              args (rest instr)]
          (case op
            "hlf" (let [reg (register (first args))
                        old (registers reg)
                        nu (int (/ old 2))]
                    (recur (inc pos) (assoc registers reg nu)))

            "tpl" (let [reg (register (first args))
                        old (registers reg)
                        nu (* old 3)]
                    (recur (inc pos) (assoc registers reg nu)))

            "inc" (let [reg (register (first args))
                        old (registers reg)
                        nu (inc old)]
                    (recur (inc pos) (assoc registers reg nu)))

            "jmp" (let [offset (read-string (first args))]
                    (recur (+ pos offset) registers))

            "jie" (let [reg (register (first args))
                        offset (read-string (second args))]
                    (if (even? (registers reg))
                      (recur (+ pos offset) registers)
                      (recur (inc pos) registers)))

            "jio" (let [reg (register (first args))
                        offset (read-string (second args))]
                    (if (= 1 (registers reg))
                      (recur (+ pos offset) registers)
                      (recur (inc pos) registers)))))))))

(println "Test 1:" ((solve-1 "test-input" 0 0) "a"))
(println "Solution 1:" ((solve-1 "input" 0 0) "b"))
(println "Solution 2:" ((solve-1 "input" 1 0) "b"))
