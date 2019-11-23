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

(test no-nest-anons
  (signals error
    (read-clojure-from-string "#(#())")))

(test autogensym
  (destructuring-bind (sym val)
      #_(eval `(let [x# 1] (list 'x# x#)))
    (is (null (symbol-package sym)))
    (is (eql val 1))))

(test function-literal
  (is (= 1 (funcall #_#(do %) 1)))
  (is (= 1 #_(#(do %) 1)))
  (is (equal '(1) #_(#(list %) 1)))
  (is (equal '((1 2 3)) #_(#(list %&) 1 2 3)))
  (is (equal '(1 (2 3)) #_(#(list % %&) 1 2 3))))

(test deref-syntax
  (is (equal '(|clojure.core|:|deref| :|x|)
             #_'@:x)))

(test fn-destructure
  (is (equal '(1 2 3)
             (funcall #_(fn [[x y z]] (list x y z))
                      '(1 2 3)))))

(test fn-lisp-1
  (is (eql -1
           (funcall #_(fn [x y] (x y))
                    #'- 1))))

(test equality
  (is (clojure= 0 0))
  (is (clojure= (#_hash 0) (#_hash 0)))
  (is (not= 0 1))
  (is (not= (#_hash 0) (#_hash 1))))

(progn
  #_(defmulti factorial identity)
  #_(defmethod factorial 0 [_]  1)
  #_(defmethod factorial :default [num]
      (* num (factorial (dec num)))))

;; (test defmulti-identity
;;   (is (= 1 (#_factorial 0)))
;;   (is (= 1 (#_factorial 1)))
;;   (is (= 6 (#_factorial 3)))
;;   (is (= 5040 (#_factorial 7))))

(progn
  #_(defmulti rand-str
        (fn [] (> (rand) 0.5)))

  #_(defmethod rand-str true
      [] "true")

  #_(defmethod rand-str false
      [] "false"))

(test defmulti-random
  (loop repeat 5 do
    (is (member (#_rand-str) '("false" "true") :test #'equal))))

(test quoted-literals
  (is (equal #_'(nil) #_(list nil)))
  (is (equal #_'(true) #_(list true)))
  (is (equal #_'(false) #_(list false))))

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
