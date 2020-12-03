(ns santa.2020.day2)

;; convert group like `\b [\b \b]` into `{"b" 2}`
(defn group->map [g] (hash-map (str (first g)) (count (second g))))

;; return letter frequency of the given string:
;; (str->freqs "alfabeta") => {"a" 3, "l" 1, "f" 1, "b" 1, "e" 1, "t" 1}
(defn str->freqs [s] (into {} (map group->map (group-by identity s))))

(defn parse-expect [expect] (map read-string (clojure.string/split expect #"-")))

(defn parse-letter [letter] (first (clojure.string/split letter #":")))

(defn positions [string] (zipmap (range 1 (+ 1 (count string))) (map str string)))

;; convert input row like `1-3 a: abcde` into
;; {:letter "a" :min 1 :max 3 :counts {"a" 1}
(defn input->edn [input]
  (let [[expect letter string] input]
    (let [[lo hi] (parse-expect expect)]
      {:letter (parse-letter letter)
       :lo lo
       :hi hi
       :pos (positions string)
       :counts (str->freqs string)})))

(defn valid-password-by-letter-count? [pass]
  (let [c ((:counts pass) (:letter pass))]
    (if c
      (and (>= c (:lo pass)) 
           (<= c (:hi pass)))
      false)))

(defn valid-password-by-letter-position? [pass]
  (let [letter (:letter pass)
        lo ((:pos pass) (:lo pass))
        hi ((:pos pass) (:hi pass))]
    (or (and (= letter lo) (not= letter hi))
        (and (not= letter lo) (= letter hi)))))

(defn count-valid-password-by-letter-count [input]
  (count (filter valid-password-by-letter-count? (map input->edn input))))

(defn count-valid-password-by-letter-position [input]
  (count (filter valid-password-by-letter-position? (map input->edn input))))
