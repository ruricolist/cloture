(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq)
  (:shadowing-import-from :fset :map))
(in-package :cloture.test)

(def-suite cloture)
(in-suite cloture)

(defun run-cloture-tests ()
  (run! 'cloture))

(defun clj (string)
  (let ((*readtable* (find-readtable 'cloture))
        (*package* (find-package "user")))
    (read-from-string string)))

(test read-vector
  (is (equal? (seq 1 2 3 :x)
              (clj "[1 2 3 :X]"))))

(test read-map
  (is (equal? (map (:x 1) (:y 2) (:z 3))
              (clj "{:X 1 :Y 2 :Z 3}"))))

(test read-meta
  (let ((sym (clj "^:dynamic *bar*")))
    (is (symbolp sym))
    (is-true (meta-ref sym :|dynamic|))))

(test let
  (is (= 3
         (eval
          (clj "(let [x 1 y 2] (+ x y))")))))

(test commas
  (is (equal '(:x :y :z) (clj "(:X, :Y, :Z)"))))

(test qq
  (is (equal '(:x) (eval (clj "`(~:X)")))))

(test reader-conditional
  (is (null (clj "#?(:clj 1)")))
  (is (eql 1 (clj "#?(:cl 1 :clj 2)"))))
