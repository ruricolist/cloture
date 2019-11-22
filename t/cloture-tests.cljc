(ns cloture.tests
  (:require [clojure.test :refer :all]))

(deftest empty-test)

(deftest trivial-test
  (is true))

;;; Examples from the clojure.test documentation.

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(deftest subtraction
  (is (= 1 (- 4 3)))
  (is (= 3 (- 7 4))))

(deftest arithmetic
  (addition)
  (subtraction))

(deftest sanity-check
  (is true)
  (is (not false))
  (is (not nil))
  (is '())
  (is (not= nil '()))
  (is (= 0 0))
  (is (not= 0 1))
  (is (not= 1 0))
  (is (= 1 1))
  (is (not= 1 -1)))

(deftest read-vector
  (is (= '(1 2 3 4) [1 2 3 4]))
  (is (not= '(1 2 3) [1 2 3 4]))
  (is (not= '(1 2 3 4) [1 2 3]))
  (is (not= '(2 3 4) [1 2 3 4]))
  (is (not= '(1 2 3 4) [2 3 4])))
