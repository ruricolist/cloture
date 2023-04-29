(defpackage :cloture.test
  (:use :cl :alexandria :serapeum :fiveam :cloture :named-readtables)
  (:import-from :fset :equal? :seq :convert)
  (:shadowing-import-from :fset :map :set)
  (:export :run-cloture-tests))
(in-package :cloture.test)
(in-readtable clojure-shortcut)

(defun run-cloture-tests ()
  (let ((result
          (|clojure.test|:|run-tests| (find-package "cloture.tests"))))
    (when (and (zerop (fset:lookup result :|pass|))
               (zerop (fset:lookup result :|fail|)))
      (error "No tests run"))))
