(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun-1 #_print-throwable (c)
  (print c))

(defun-1 #_root-cause(c) c)

(defun-1 #_print-cause-trace (c &optional n)
  (#_print-stack-trace c n))

(defun-1 #_print-stack-trace (tr &optional n)
  (uiop:print-backtrace :count n :condition tr))
