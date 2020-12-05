(ns santa.2020.day4)
(require '[clojure.string :as str])

;; hell's bells 
(def numbermap {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "0" 0})
(def numbers (set (keys numbermap)))

(def eye-colors (set ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(def hair-colors (into numbers (set ["#" "a" "b" "c" "d" "e" "f"])))

(def expected-fields (set ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn is-number? [n] (if (numbermap n) :number :letter))

(defn parse-height [h]
  (let [p (group-by is-number? (map str h))]
    {:number (read-string (apply str (:number p)))
     :unit (apply str (:letter p))}))

(defn birth-year? [byr]
  (let [y (read-string byr)]
    (<= 1920 y 2002)))

(defn issue-year? [iyr]
  (let [y (read-string iyr)]
    (<= 2010 y 2020)))

(defn expiration-year? [eyr]
  (let [y (read-string eyr)]
    (<= 2020 y 2030)))

(defn height? [hgt]
  (let [h (parse-height hgt)]
    (case (:unit h)
      "cm" (<= 150 (:number h) 193)
      "in" (<= 59 (:number h) 76)
      "" false)))

(defn hair-color? [color]
  (not (nil? (re-matches #"#[a-f0-9]{6}" color))))

(defn eye-color? [color] (contains? eye-colors color))

(defn passport-id? [pid]
  (and 
    (= (count pid) 9) 
    (into [] (filter #(contains? numbers %) (map str pid)))))

(defn passport->map 
  "Convert passport line into hash-map."
  [p]
  (into {} (for [l p] (let [p (str/split l #":")] (hash-map (first p) (second p))))))

(defn lines->map 
  "Convert input lines of text into lists of hash-maps."
  [input] 
  (map passport->map 
    (map #(str/split % #" ")
       (map #(str/join " " %) 
            (filter #(not= "" (first %)) (partition-by #(= % "") input))))))

(defn valid-passport? 
  "Return true if all expected keys are present in passport map, false otherwise."
  [pm] 
  (and 
    (= 0 (count (into [] (for [e expected-fields :when (not (pm e))] 1))))
    (birth-year? (pm "byr"))
    (issue-year? (pm "iyr"))
    (expiration-year? (pm "eyr"))
    (height? (pm "hgt"))
    (hair-color? (pm "hcl"))
    (eye-color? (pm "ecl"))
    (passport-id? (pm "pid"))))

;; I guess this is a little bit of cheating.
;; Original lines were converted in editor into this form for easier parsing
;; later.
(def test-input-4
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
"byr:1937 iyr:2017 cid:147 hgt:183cm"
""
"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
"hcl:#cfa07d byr:1929"
""
"hcl:#ae17e1 iyr:2013"
"eyr:2024"
"ecl:brn pid:760753108 byr:1931"
"hgt:179cm"
""
"hcl:#cfa07d eyr:2025 pid:166559648"
"iyr:2011 ecl:brn hgt:59in"])

(println (count (filter valid-passport? (lines->map test-input-4))))

