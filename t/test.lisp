(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq :convert)
  (:shadowing-import-from :fset :map :set))
(in-package :cloture.test)
(in-readtable clojure-shortcut)

(def-suite cloture)
(in-suite cloture)

(defun run-cloture-tests ()
  (run! 'cloture)
  (|clojure.test|:|run-tests| (find-package "cloture.tests")))

(defmacro is-truthy? (condition &rest reason-args)
  `(if (truthy? ,condition)
       (5am::add-result '5am::test-passed :test-expr ',condition)
       (5am::process-failure ',condition
                             ,@(or reason-args
                                   `("~S did not return a truthy value" ',condition)))))

(defmacro is-falsy? (condition &rest reason-args)
  (with-gensyms (value)
    `(let ((,value ,condition))
       (if (truthy? ,value)
           (5am::process-failure ',condition
                                 ,@(or reason-args
                                       `("~S returned the value ~S, which Clojure considers truthy" ',condition ,value)))
           (5am::add-result '5am::test-passed :test-expr ',condition)))))

(test lazy-seq-equality
  (is-truthy? #_(empty? (lazy-seq '())))
  (is-falsy? #_(seq (lazy-seq '())))
  (is-falsy? #_(seq (lazy-seq nil)))
  (is (clojure= #_(lazy-seq (cons 1 (lazy-seq '(2))))
                '(1 2)))
  (is-falsy?
   #_(= (lazy-seq (cons 1 (lazy-seq '(2)))) '(1 3)))
  (is (clojure= #_(lazy-seq (cons 1 (lazy-seq '(2))))
                #_(lazy-seq (cons 1 (lazy-seq '(2))))))
  (is (not= #_(lazy-seq (cons 1 (lazy-seq '(2))))
            #_(lazy-seq (cons 1 (lazy-seq '(3))))))
  (is-truthy?
   #_(let [tail (lazy-seq (list 2 3))
           seq (lazy-seq (list 1 tail))]
       (= '(1) seq)
       (not (realized? tail)))))

#_(defn squares-odd [n]
    (cons (* n n) (lazy-seq (squares-odd (inc n)))))
#_(defn squares-even [n]
    (lazy-seq (cons (* n n) (squares-even (inc n)))))

(test lazy-seq
  (is (clojure= #_(take 1 (squares-odd 1))
                #_(take 1 (squares-even 1)))))

(test cycle
  (is (clojure= '(1 2 3 1 2 3 1 2 3 1)
                #_(take 10 (cycle '(1 2 3))))))

(test concat
  (is (clojure= '(1 2 3 4 5 6)
                #_(concat '(1 2 3) '(4 5 6)))))

(test take
  (is (clojure= '(1 2 3 4 5 6)
                #_(take 6 '(1 2 3 4 5 6)))))

(test repeat
  (is (clojure= '(1 1 1 1 1)
                #_(repeat 5 1)))
  (is (clojure= '(1 1 1)
                #_(repeat 3 1))))

(test filter
  (is (clojure= '(0 2 4 6 8)
                #_(filter even? (range 10)))))

(test map
  (is (clojure= (cl:map 'list #'- (range 5))
                #_(map - (range 5))))
  (is (clojure= (cl:map 'list #'- (range 5) (range 5 10))
                #_(map - (range 5) (range 5 10)))))

(test range
  (is-truthy? #_(empty? (take 10 (range 0 0 0))))
  (is (clojure= (make-list 5 :initial-element 0)
                #_(take 5 (range 0 10 0))))
  (is (clojure= (serapeum:range 10)
                #_(take 10 (range)))))

(test drop-while
  (is (clojure= '(2 4 6)
                #_(drop-while odd? '(1 3 5 2 4 6)))))

(test interpose
  (is-truthy?
   #_(= '("one" "," "two" "," "three")
        (interpose "," '("one" "two" "three")))))

(test group-by
  (let ((map #_(group-by count ["a" "as" "asd" "aa" "asdf" "qwer"])))
    (is (= (fset:size map) 4))))

(test doseq
  (is (equal "123" #_(with-out-str (doseq [x '(1 2 3)] (pr x))))))

(test assoc-in
  (is-truthy?
   #_(= {:x {:y {:z 1}}} (assoc-in {} [:x :y :z] 1))))

(test macroexpand
  (is-truthy?
   #_(= '(def x (fn x []))
        (macroexpand '(defn x [])))))

(test empty?
  (is-truthy? #_(empty? '()))
  (is-truthy? #_(empty? nil)))
