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

