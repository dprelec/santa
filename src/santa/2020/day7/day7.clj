(ns santa.2020.day7
 (:require [clojure.string :refer [split split-lines]]))

(defn parse-bag [bag]
  (let [f (re-find #"(\d+)\s*([a-zA-Z ]+)\s+bag" bag)]
    (if (seq f)
      {(f 2) (read-string (f 1))})))

(defn parse-line [line]
  (let [parts (split line #" bags contain ")
        bags (remove nil? (map parse-bag (split (second parts) #", ")))]
    (if (seq bags)
      {(first parts) (keys (into {} bags))})))

(defn mapvals->set [m] (into {} (for [k m] {(key k) (set (val k))})))

(defn parse-lines->map [input-file]
  (mapvals->set 
    (loop [m (remove nil? (map parse-line (split-lines (slurp input-file)))) d {}]
      (if-not (seq m)
        d
        (recur (rest m) (merge-with concat (first m) d))))))

(defn traverse-colors [color-map colors]
  (remove nil? (for [k color-map c colors]
                 (if (some #(= % c) (val k)) (key k)))))

(defn traverse [color-map colors]
  (loop [c colors cnt '()]
    (let [t (traverse-colors color-map c)]
      (if-not (seq t)
        (count (set cnt))
        (recur t (concat cnt t))))))

;; solution to part 1
(println (traverse (parse-lines->map "test-input") #{"shiny gold"}))
(println (traverse (parse-lines->map "input") #{"shiny gold"}))

(defn parse-color-line [line]
  (let [parts (split line #" bags contain ")
        bags (remove nil? (map parse-bag (split (second parts) #", ")))]
    (if (seq bags)
      {(first parts) (into {} bags)})))

(defn parse-lines->colormap [input-file]
  (into {} (remove nil? (map parse-color-line (split-lines (slurp input-file))))))

(defn count-bags [colormap color]
  (let [nodes (colormap color)]
    (if-not (seq nodes)
      0
      (apply + (for [kv nodes] (+ (val kv) (* (val kv) (count-bags colormap (key kv)))))))))

;; solution to part 2
(println (count-bags (parse-lines->colormap "test-input") "shiny gold"))
(println (count-bags (parse-lines->colormap "input") "shiny gold"))
