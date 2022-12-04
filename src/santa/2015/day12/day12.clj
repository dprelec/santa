(ns day12 (:require [clojure.string :refer [join split-lines index-of]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(defn solve-1 [input-file]
  (let [input (input->vec input-file)
        nums (map read-string (re-seq #"[-0-9]+" (first input)))]
    (apply + nums)))

;(println (solve-1 "input"))

(def invert-brace {"{" "}" "}" "{"})

(defn solve-2 [input-file]
  (let [input (first (input->vec input-file))
        reverse-input (join (reverse input))
        index-of-brace (fn [input brace-type start-pos]
          (loop [idx start-pos braces 0]
            (if (and (> idx start-pos) (zero? braces))
              idx
              (let [c (subs input idx (inc idx))]
                (if (= c (invert-brace brace-type))
                  (recur (inc idx) (inc braces))
                  (if (= c (invert-brace (invert-brace brace-type)))
                    (recur (inc idx) (dec braces))
                    (recur (inc idx) braces)))))))]
    (loop [entries [] pos 0]
      (let [idx (index-of input ":\"red" pos)]
        (if (nil? idx)
          entries
          (let [next-close (index-of-brace input "}" idx)
                prev-close (index-of-brace reverse-input "{" (- (count input) idx))
                prev (- (count input) prev-close)]
            (recur (conj entries 
                         [prev idx next-close (subs input prev next-close)]) 
                   (+ 5 idx))))))))

(println (solve-2 "input"))

