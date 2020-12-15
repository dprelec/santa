(ns santa.2020.day14
 (:require [clojure.string :refer [split-lines split]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

(def bit-map {\0 0 \1 1})

(defn vec->mask [[k v]] {k (bit-map v)})

(defn parse-mask->map [mask]
  (apply merge (map vec->mask (map-indexed vector (reverse mask)))))

(defn bit-write [n i b]
  (if (and (= b 1) (not (bit-test n i)))
    (bit-set n i)
    (if (and (= b 0) (bit-test n i))
      (bit-flip n i)
      n)))

(defn apply-mask [n m]
  (loop [m m n n]
    (if-not (seq m)
      n
      (let [[i b] (first m)]
        (if (nil? i)
          (recur (rest m) n) ; skip X
          (recur (rest m) (bit-write n i b)))))))

(defn parse-op [[op eq vl]]
  (if (= op "mask") 
    {:op op :val (parse-mask->map vl) :mask vl}
    (let [r (re-matches #"mem\[(\d+)\]" op)]
      (if (= 2 (count r))
        {:op "mem" :val (read-string vl) :idx (read-string (second r))}
        {:op nil}))))

(defn parse-input [input]
  (map parse-op (map #(split % #" ") input)))

(defn execute [input]
  (loop [input input mask "" acc {}]
    (if-not (seq input)
      acc
      (let [op (first input)]
        (if (= (:op op) "mask")
          (recur (rest input) (:val op) acc)
          (if (= (:op op) "mem")
            (recur (rest input) mask (assoc acc (:idx op) (apply-mask (:val op) mask)))))))))

;; solution to part 1
;(println (apply + (vals (execute (parse-input (input->vec "input"))))))

;; because format sucks
(defn lpad [s n] (let [p (- n (count s))] (str (clojure.string/join (repeat p "0")) s)))

(defn shift [[n m]] (if (= \0 m) n (if (= \1 m) 1 (if (= \X m) \X))))

(defn apply-shift [n m]
  (clojure.string/join (map str (map shift (partition 2 2 (interleave n m))))))

(defn bins [c] 
  (let [b (for [i (range (int (Math/pow 2 c)))] (Integer/toBinaryString i))
        m (apply max (map count b))]
    (map #(lpad % m) b)))

(defn patch [m b]
  (let [indexed (apply merge (map-indexed hash-map m))
        positions (map first (filter #(= \X (val %)) indexed))
        merged (partition 2 2 (interleave positions b))]
    (loop [indexed indexed merged merged]
      (if-not (seq merged)
        (clojure.string/join (map #(str (second %)) (sort-by #(key %) indexed)))
        (let [[pos vl] (first merged)]
          (recur (assoc indexed pos (str vl)) (rest merged)))))))

(defn generate-masks [m]
  (into [] (let [c (count (filter #(= \X %) m))]
    (for [b (bins c)]
      (patch m b)))))

;; enterprise-grade stuff
(defn mask->dec [m]
  (let [mi (map-indexed vector (map read-string (map str (reverse m))))
        mp (map #(* (second %) (Math/pow 2 (first %))) mi)]
    (bigint (apply + mp))))

(defn calc-indices [n m]
  (map mask->dec (generate-masks (apply-shift (lpad (Integer/toBinaryString n) 36) m))))

(defn assoc-indices [m i v]
  (loop [i i m m]
    (if-not (seq i)
      m
      (recur (rest i) (assoc m (first i) v)))))

(defn execute-masks [input]
  (loop [input input mask "" acc {}]
    (if-not (seq input)
      acc
      (let [op (first input)]
        (if (= (:op op) "mask")
          (recur (rest input) (:mask op) acc)
          (if (= (:op op) "mem")
            (let [indices (calc-indices (:idx op) mask)]
              (recur (rest input) mask (assoc-indices acc indices (:val op))))))))))

;; solution to part 2
(println (apply + (vals (execute-masks (parse-input (input->vec "test-input-2"))))))
(println (apply + (vals (execute-masks (parse-input (input->vec "input"))))))
