(ns santa.core-test
  (:require [clojure.test :refer :all]
            [santa.mmxx.core :refer :all]))

(def test-input-1 [1721 979 366 299 675 1456])

(def test-output-1 514579) 
(def test-output-2 241861950)

(deftest test-problem-1-1
  (testing "Testing problem 1-1."
    (is (= (day-1-1 test-input-1) test-output-1))))

(deftest test-problem-1-2
  (testing "Testing problem 1-2."
    (is (= (day-1-2 test-input-1) test-output-2))))

(def test-input-2-1 
  [["1-3" "a:" "abcde"]
   ["1-3" "b:" "cdefg"]
   ["2-9" "c:" "ccccccccc"]])

(deftest test-parse-expect
  (testing "parse-expect"
    (is (= (parse-expect "1-2") '(1 2)))))

(def test-problem-2-1
  (is (= (count-valid-password-by-letter-count test-input-2-1) 2)))

(def test-problem-2-2
  (is (= (count-valid-password-by-letter-position test-problem-2-1) 1)))

