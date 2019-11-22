(ns cloture.tests
  (:require [clojure.test :refer :all]))

(deftest empty-test)

(deftest trivial-test
  (is true))

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(deftest subtraction
  (is (= 1 (- 4 3)))
  (is (= 3 (- 7 4))))

(deftest arithmetic
  (addition)
  (subtraction))
