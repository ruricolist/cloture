(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun-1 #_intersection (&rest sets)
  (reduce #'fset:intersection sets))

(defun-1 #_union (&rest sets)
  (reduce #'fset:union sets))

(defun-1 #_subset? (set1 set2)
  (fset:subset? set1 set2))

(defun-1 #_superset? (set1 set2)
  (#_subset? set2 set1))

(defun-1 #_select (pred set)
  (fset:filter (ifn-function pred) set))
