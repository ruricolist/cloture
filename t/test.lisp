(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq :convert)
  (:shadowing-import-from :fset :map :set)
  (:export :run-cloture-tests :run-cloture-test))
(in-package :cloture.test)
(in-readtable clojure-shortcut)

(defun run-cloture-tests ()
  (let ((result
          (|clojure.test|::|run-tests| (find-package "cloture.tests"))))
    (when (and (zerop (fset:lookup result :|pass|))
               (zerop (fset:lookup result :|fail|)))
      (error "No tests run"))))

(defun run-cloture-test (test-name)
  (let ((cl-sym
          (or (find-symbol (string test-name) "cloture.tests")
              (error "No such test: ~a" test-name))))
    (|clojure.test|::|test-var| (|clojure.core|:|find-var| cl-sym))))
