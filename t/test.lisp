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

(test assoc-in
  (is-truthy?
   #_(= {:x {:y {:z 1}}} (assoc-in {} [:x :y :z] 1))))

(test macroexpand
  (is-truthy?
   #_(= '(def x (fn x []))
        (macroexpand '(defn x [])))))
