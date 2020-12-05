(ns santa.2020.day5)

;; letter to binary mapping
(def letter->bin {"F" 0 "B" 1 "L" 0 "R" 1})

(def rowset (set ["F" "B"]))

(defn row? [r] (if (contains? rowset r) :row :col))

(defn parse-path [d] (group-by row? (map str d)))

(defn binary->decimal [b] (Integer/parseInt b 2))

(defn direction->decimal [d]
  (binary->decimal (apply str (map letter->bin d))))

(defn direction->row-id [d]
  (let [p (parse-path d)
        r (direction->decimal (:row p))
        c (direction->decimal (:col p))]
    (+ (* 8 r) c)))

(defn highest-seat-id [input]
  (first (sort #(compare %2 %1) (map direction->row-id input))))

(def test-input 
  ["BFFFBBFRRR"
   "FFFBBBFRRR"
   "BBFFBBFRLL"
   "FBFBBFFRLR"])

(println (time (highest-seat-id test-input)))
