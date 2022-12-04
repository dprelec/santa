(ns santa.2020.day18.day18
 (:require [clojure.string :refer [split-lines]]))

(defn parse-line [line]
  (loop [line (map str line) tokens []]
    (if-not (seq line)
      (remove nil? tokens)
      (let [token (first line)
            parse (case token 
                    "+" {:op :F :fn +} 
                    "-" {:op :F :fn -} 
                    "*" {:op :F :fn *} 
                    "/" {:op :F :fn /}
                    "(" {:op :S}
                    ")" {:op :E}
                    " " nil
                        {:op :N :num (read-string token)})]
        (recur (rest line) (conj tokens parse))))))

(defn sub-expr [expr]
  (loop [e (rest expr) brackets 1 idx 0]
    (if (zero? brackets)
      (drop 1 (take idx (butlast expr)))
      (case (:op (first e))
        :S (recur (rest e) (inc brackets) (inc idx))
        :E (recur (rest e) (dec brackets) (inc idx))
           (recur (rest e) brackets       (inc idx))))))

(defn evaluate-expr [expr]
  (loop [expr expr stack [] fun nil]
    (if-not (seq expr)
      (apply fun stack)
      (let [op (first expr)]
        (case (:op op)
          :S (let [e (sub-expr expr)
                   r (evaluate-expr e)]
               (recur (drop (inc (count e)) (rest expr)) (conj stack r) fun))
          :F (if (= 2 (count stack)) 
               (recur (rest expr) [(apply fun stack)] (:fn op))
               (recur (rest expr) stack               (:fn op)))
          :N (recur (rest expr) (conj stack (:num op)) fun))))))

(defn solution-1 [input-file]
  (apply + (map #(evaluate-expr (parse-line %)) (split-lines (slurp input-file)))))

(println (solution-1 "input"))
