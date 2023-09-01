(ns day21 (:require [clojure.string :refer [replace split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(def operation {"*" *
                "/" /
                "-" -
                "+" +})

(defn evaluate [m expr]
  ;  (println {:eval expr})
  (case (:type expr)
    :value (:val expr)
    :expr (let [op (operation (:op expr))
                o1 (m (:o1 expr))
                o2 (m (:o2 expr))
                a1 (evaluate m o1)
                a2 (evaluate m o2)]
            (op a1 a2))))

(defn expr-map [row]
  (let [parts (split row #" ")]
    (if (> (count parts) 2)
      (let [ky (replace (nth parts 0) ":" "")
            o1 (nth parts 1)
            o2 (nth parts 3)
            op (nth parts 2)]
        {ky {:type :expr :op op :o1 o1 :o2 o2}})
      (let [ky (replace (nth parts 0) ":" "")
            vl (read-string (nth parts 1))]
        {ky {:type :value :val vl}}))))

(defn solve1 [input-file]
  (let [input (input->vec input-file)
        evalmap (apply merge (map expr-map input))]
    ;(println {:evalmap evalmap})
    (evaluate evalmap (evalmap "root"))))

;; initial value for h manually determined :)
(defn solve2 [input-file]
  (let [input (input->vec input-file)
        evalmap (apply merge (map expr-map input))
        rootleft (:o1 (evalmap "root"))
        rootright (:o2 (evalmap "root"))]
    ;(println {:evalmap evalmap})
    (loop [h 3587647562500]
      (let [e (assoc evalmap "humn" {:type :value :val h})
            l (evaluate e (e rootleft))
            r (evaluate e (e rootright))]
        (println {:h h :l l :r r :diff (double (- l r))})
        (if-not (= l r)
          (recur (+ 1 h))
          {:h h :l l :r r})))))

;(println (solve2 "test-input"))

(println (solve2 "input"))

;(println "Test 1:" (solve1 "test-input"))
;(println "Solution 1:" (solve1 "input"))
