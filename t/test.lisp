(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq :convert)
  (:shadowing-import-from :fset :map :set)
  (:export :run-cloture-tests))
(in-package :cloture.test)
(in-readtable clojure-shortcut)

(defun counter (result key)
  (or (fset:lookup result key) 0))

(defun run-cloture-tests ()
  "Run the suite. Signals an error unless every test passed, so that
`asdf:test-system' exits non-zero on a red suite."
  (let* ((result (|clojure.test|:|run-tests| (find-package "cloture.tests")))
         (pass (counter result :|pass|))
         (fail (counter result :|fail|))
         (errors (counter result :|error|)))
    (when (and (zerop pass) (zerop fail) (zerop errors))
      (error "No tests run"))
    (unless (and (zerop fail) (zerop errors))
      (error "~a failure~:p and ~a error~:p in ~a assertion~:p."
             fail errors (+ pass fail errors)))
    result))
