(in-package #:cloture)
(in-readtable clojure-shortcut)

(declaim (inline #_aref #_expt))

(defun-1 #_funcall (fn &rest args)
  (ifn-apply fn args))

(define-clojure-macro #_declare-keywords (&rest keywords)
  `(declare-keywords ,@keywords))

(defun-1 #_aref (array &rest subscripts)
  (apply #'aref array subscripts))

(defun-1 #_expt (b e)
  (expt b e))

(defun-1 #_parse-integer (string &key
                                 (start 0)
                                 end
                                 (radix 10)
                                 (junk-allowed false))
  (multiple-value-call #'parse-integer string
    :start start
    (if (nil? end)
        (values)
        (values :end end))
    :radix radix
    :junk-allowed (truthy? junk-allowed)))
