(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq :convert)
  (:shadowing-import-from :fset :map :set))
(in-package :cloture.test)
(in-readtable clojure-shortcut)

(def-suite cloture)
(in-suite cloture)

(defun run-cloture-tests ()
  (run! 'cloture))

(test read-vector
  (is (equal? (seq 1 2 3 :x) '#_[1 2 3 :X])))

(test read-map
  (is (equal? (map (:x 1) (:y 2) (:z 3))
              #_{:X 1 :Y 2 :Z 3})))

(test read-meta
  (let ((sym '#_^:dynamic *bar*))
    (is (symbolp sym))
    (is-true (meta-ref sym :|dynamic|))))

(test let
  (is (= 3 #_(let [x 1 y 2] (+ x y)))))

(test commas
  (is (equal '(:x :y :z) '#_(:X, :Y, :Z))))

(test qq
  (is (equal '(:x) #_`(~:X))))

(test reader-conditional
  (is (null #_#?(:clj 1)))
  (is (eql 1 #_#?(:cl 1 :clj 2))))

(test destructure-simple
  (is (equal '(1 2 3) #_(let ([x y z] [1 2 3])
                          (list x y z))))

  (is (equal '(1 2 3) #_(let ([x y z] '(1 2 3))
                          (list x y z)))))

(test destructure-as
  (is (equal '(1 2 3)
             (convert 'list
                      #_(let ([_ _ _ :as all] [1 2 3])
                          all))))
  (is (equal '(1 2 3)
             (convert 'list
                      #_(let ([_ _ _ :as all] '(1 2 3))
                          all)))))

(test destructure-rest
  (is (equal '(2 3) #_(let ([_ & ys] [1 2 3])
                        ys)))
  (is (equal '(2 3) #_(let ([_ & ys] '(1 2 3))
                        ys))))

(test destructure-rest-and-as
  (is (equal '(1 2 3)
             (convert 'list
                      #_(let ([_ & _ :as all] [1 2 3])
                          all))))
  (is (equal '(1 2 3)
             (convert 'list
                      #_(let ([_ & _ :as all] '(1 2 3))
                          all)))))

(test destructure-nested
  (is (equal '(1 2 3 4 5 6)
             #_(let [[[a] [[b]] c [x y z]] [[1] [[2]] 3 [4 5 6]]]
                 (list a b c x y z)))))

(test destructure-short
  (is (equal '(nil nil nil)
             #_(let [[x y z] '()]
                 (list x y z)))))

(test destructure-lisp-vector
  (is (equalp #(1 2 3 4 5)
              #_(let [[_ _ _ _ _ :as all] (CL:VECTOR 1 2 3 4 5)]
                  all)))
  (is (equal '(1 2 3 4 5)
             #_(let [[a b c d e] (CL:VECTOR 1 2 3 4 5)]
                 (list a b c d e)))))

(test fn
  (let ((bar
          #_(fn bar
              ([a b]
               (bar a b 100))
              ([a b c]
               (* a b c)))))
    (is (= 3000 (funcall bar 5 6)))
    (is (= 60 (funcall bar 5 6 2)))))

(let ((bar
        #_(fn bar
            ([a b]
             (bar a b 100))
            ([a b c]
             (* a b c)))))
  (= 3000 (funcall bar 5 6))
  (= 60 (funcall bar 5 6 2)))

(test ->
  (is (listp (#_-> '((1 2) (3 4)))))
  (is (= 2 (#_-> '((1 2) (3 4)) first second)))
  (is (= 3 (#_-> '((1 2) (3 4)) second first))))

(test ->>
  (= 3/4 (#_->> 5 (+ 3) (/ 2) (- 1))))

(test loop-recur
  (let ((fact
          #_(fn [n]
              (loop [cnt n
                         acc 1]
                    (if (zero? cnt)
                        acc
                        (recur (dec cnt) (* acc cnt)))))))
    (= (funcall fact 10)
       (factorial 10))))

;;; You need `private' to prevent package variance on SBCL.
#_(def ^{:dynamic true :private true} foo* 0)
#_(defn ^:private get-foo [] foo*)

(test binding
  (is (= 0 #_foo*))
  (is (= 1 #_(let [foo* 1] foo*)))
  (is (= 0 #_(let [foo* 1] (get-foo))))
  (is (= 1 #_(binding [foo* 1] foo*)))
  (is (= 1 #_(binding [foo* 1] (get-foo)))))

(test var
  (is (eql* (macroexpand-1 '#_foo*)
            #_(var foo*)
            #_#'foo*)))

(test set
  (is (typep #_#{1 2 3} 'set)))

(test seq
  (is (eql #_nil #_(seq '())))
  (is (eql #_nil #_(seq {})))
  (is (eql #_nil #_(seq #{})))
  (is (eql #_nil #_(seq [])))
  (is (set-equal '(1 2 3) #_(seq #{1 2 3})))
  (is (equal? (list (seq :x 1)
                    (seq :y 2))
              #_(seq {:X 1 :Y 2}))))

(test letfn
  (is (= 1
         #_(letfn [(first [xs] (CL:FIRST xs))]
             (first '(1 2 3))))))

(test read-nothing
  (is (equal '(1 2) '#_(1 2 #_3))))

;; (test autogensym
;;   (destructuring-bind (x1 x2)
;;       #_`(~x# ~x#)
;;     (is (null (symbol-package x1)))
;;     (is (eql x1 x2))))
