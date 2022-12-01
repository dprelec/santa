(ns day3)

;; Solution to part 1 calculated manually using the script for solving
;; project euler problem no. 28.
;; This script returned for row 296 the following diagonal numbers:
;; top: 348101 - 347511 | bottom: 348691 - 349281
;;
;; Input number 347991 is located between top numbers.
;;
;; * 348691 - 347511 = 590 - width of the top row
;; * 590 / 2         = 295 - half of the width
;; * 348691 - 347991 = 110 - location of our input
;; * 295 - 110       = 185 - number of steps to the right
;; * 296 - 1 + 185   = 480 - total number of steps to the center
(def solve-1 480)

;; oeis: https://oeis.org/A141481
;; list: https://oeis.org/A141481/b141481.txt
(def solve-2 349975)

