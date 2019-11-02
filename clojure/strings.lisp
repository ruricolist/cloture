(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun-1 #_starts-with? (string prefix)
  (string^= prefix string))

(defun-1 #_ends-with? (string suffix)
  (string$= suffix string))
