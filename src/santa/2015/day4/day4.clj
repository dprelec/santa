(ns day4
  (:require [clojure.string :refer [join]]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

;; when you don't know enough of Java
;; source: https://gist.github.com/jizhang/4325757
(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn solve-1 [secret zeros]
  (let [check (join (take zeros (repeat "0")))]
    (loop [n 1]
      (let [digest (md5 (format "%s%d" secret n))
            substr (subs digest 0 zeros)]
        (if-not (= substr check)
          (recur (inc n))
          n)))))

;; test-case
(println (solve-1 "abcdef" 5))

;; part 1
(println (solve-1 "yzbqklnj" 5))

;; part 2
(println (solve-1 "yzbqklnj" 6))
