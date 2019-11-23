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

(deftest read-map
  (is (= '([:x 1] [:y 2] [:z 3])
         (seq {:x 1 :y 2 :z 3})))
  ;; Clojure's behavior differs from FSet here; worth fixing?
  ;; (is (not= '([:x 1] [:y 2] [:z 3])
  ;;           (seq {:y 2 :x 1  :z 3})))
  (is (not= '([:x 1] [:y 2])
            (seq {:x 1 :y 2 :z 3}))))

(deftest read-meta
  (let [sym (read-string "^:dynamic *bar*")]
    (is (symbol? sym))
    (is (get (meta sym) :|dynamic|))))

(deftest let-test
  (is (= 3 (let [x 1 y 2] (+ x y))))
  (is (= 3 (let [x 2 y 1] (+ x y))))
  (is (not= 3 (let [x 1 y 1] (+ x y)))))

(deftest commas
  (is (= '() (,,,,,)))
  (is (= '(:x :y :z) '(:x, :y, :z))))

(deftest qq
  (is (= '(:x) `(~:x)))
  (is (not= '(:x) `(~:X))))

(deftest reader-conditional
  (is (= 1 #?(:cl 1 :clj 2))))

(deftest destructure-simple
  (is (= '(1 2 3)
         (let [[x y z] [1 2 3]]
           (list x y z))))
  (is (not= '(1 2 3 4)
            (let [[x y z] [1 2 3]]
              (list x y z))))
  (is (not= '(1 2)
            (let [[x y z] [1 2 3]]
              (list x y z))))
  (is (= '(1 2 3)
         (let [[x y z] '(1 2 3)]
           (list x y z)))))

(deftest destructure-as
  (is (= '(1 2 3)
         (let [[_ _ _ :as all] [1 2 3]]
           all)))
  (is (= '(1 2 3)
         (let [[_ _ _ :as all] '(1 2 3)]
           all))))

(deftest destructure-rest
  (is (= '(2 3)
         (let [[_ & ys] [1 2 3]]
           ys)))
  (is (= '(2 3)
         (let [[_ & ys] '(1 2 3)]
           ys))))

(deftest destructure-rest-and-as
  (is (= '(1 2 3)
         (let [[_ & _ :as all] [1 2 3]]
           all)))
  (is (= '(1 2 3)
         (let [[_ & _ :as all] '(1 2 3)]
           all))))

(deftest destructure-nested
  (is (= '(1 2 3 4 5 6)
         (let [[[a] [[b]] c [x y z]] [[1] [[2]] 3 [4 5 6]]]
           (list a b c x y z)))))

(deftest destructure-short
  (let [l1 (list nil)
        l2 (let [[x] '()]
             (list x))]
    (is (= l1 l2))))

(deftest destructure-lisp-vector
  (is (= #=(CL:VECTOR 1 2 3 4 5)
         (let [[_ _ _ _ _ :as all] #=(CL:VECTOR 1 2 3 4 5)]
           all)))
  (is (= '(1 2 3 4 5)
         (let [[a b c d e] #=(CL:VECTOR 1 2 3 4 5)]
           (list a b c d e)))))
