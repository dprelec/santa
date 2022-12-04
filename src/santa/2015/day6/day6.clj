(ns day6 (:require [clojure.string :refer [split split-lines]]))

(defn input->vec [input-file] (split-lines (slurp input-file)))

;; return vector of coords in the form "x,y" => [x y]
(defn parse-coords [coords] (mapv read-string (split coords #",")))

(defn solve-1 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; create lights all turned off
        lights (int-array 1000000)
        ;; calculate position within the array
        calc-pos (fn [x y] (+ x (* y 1000)))
        ;; turn on lights function
        on (fn [x y] (aset lights (calc-pos x y) 1))
        ;; turn off lights function
        off (fn [x y] (aset lights (calc-pos x y) 0))
        ;; toggle lights function
        toggle (fn [x y]
                 (let [v (aget lights (calc-pos x y))]
                   (if (= 1 v)
                     (off x y)
                     (on x y))))]
    (loop [rows input]
      (if-not (seq rows)
        (count (remove zero? lights))
        (let [;; parse row into separate parts
              parts (split (first rows) #" ")
              ;; extract command: on, off, toggle
              command (case (first parts)
                        "turn" (second parts) ;; on|off
                        "toggle" (first parts))
              ;; extract coords
              args (reverse parts)
              from (parse-coords (nth args 2))
              to (parse-coords (nth args 0))]
          (doseq [x (range (from 0) (inc (to 0)))
                  y (range (from 1) (inc (to 1)))]
            (case command
              "on" (on x y)
              "off" (off x y)
              "toggle" (toggle x y)))
          (recur (rest rows)))))))

(println (solve-1 "test-input-1"))
(println (solve-1 "test-input-2"))
(println (solve-1 "test-input-3"))
(println (solve-1 "input"))


(defn solve-2 [input-file]
  (let [;; read input
        input (input->vec input-file)
        ;; create lights all turned off
        lights (int-array 1000000)
        ;; calculate position within the array
        calc-pos (fn [x y] (+ x (* y 1000)))
        ;; increase lights function
        on (fn [x y]
             (let [pos (calc-pos x y)
                   old (aget lights pos)]
               (aset lights pos (inc old))))
        ;; decrease lights function
        off (fn [x y]
              (let [pos (calc-pos x y)
                    old (aget lights pos)]
                (if (> old 0)
                  (aset lights pos (- old 1))
                  (aset lights pos 0))))
        ;; increase by 2 lights function
        toggle (fn [x y]
                 (let [pos (calc-pos x y)
                       old (aget lights pos)]
                   (aset lights pos (+ 2 old))))]
    (loop [rows input]
      (if-not (seq rows)
        (apply + lights)
        (let [;; parse row into separate parts
              parts (split (first rows) #" ")
              ;; extract command: on, off, toggle
              command (case (first parts)
                        "turn" (second parts) ;; on|off
                        "toggle" (first parts))
              ;; extract coords
              args (reverse parts)
              from (parse-coords (nth args 2))
              to (parse-coords (nth args 0))]
          (doseq [x (range (from 0) (inc (to 0)))
                  y (range (from 1) (inc (to 1)))]
            (case command
              "on" (on x y)
              "off" (off x y)
              "toggle" (toggle x y)))
          (recur (rest rows)))))))

(println (solve-2 "test-input-1"))
(println (solve-2 "test-input-2"))
(println (solve-2 "test-input-3"))
(println (solve-2 "input"))
