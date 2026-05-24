(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Clojure arrays are represented by Lisp arrays.

;;; TODO Should we represent multidimensional arrays by nested arrays
;;; or actual multidimensional arrays?

(defun-1 #_aclone (array)
  (copy-array array))

(defun-1 #_to-array (coll)
  (lret ((arr (make-array (#_count coll))))
    (iter (for x in-seq coll)
      (for i from 0)
      (setf (aref arr i) x))))

(defun-1 #_alength (arr)
  (array-dimension arr 0))
