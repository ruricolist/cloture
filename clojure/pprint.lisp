(in-package #:cloture)
(in-readtable clojure-shortcut)

(expose-to-clojure
  #_clojure.pprint/*print-base*
  *print-base*
  #_clojure.pprint/*print-miser-width*
  *print-miser-width*
  #_clojure.pprint/*print-pprint-dispatch*
  *print-pprint-dispatch*
  #_clojure.pprint/*print-pretty*
  *print-pretty*
  #_clojure.pprint/*print-radix*
  *print-radix*
  #_clojure.pprint/*print-right-margin*
  *print-right-margin*)

(defun-1 #_clojure.pprint:cl-format (writer format-in &rest args)
  (format writer "~?" format-in args))

(defmacro #_clojure.pprint:formatter (format-in)
  `(formatter ,format-in))

(define-compiler-macro #_clojure.pprint:cl-format (&whole call writer format-in &rest args)
  (if (stringp format-in) call
      `(#_clojure.pprint/cl-format ,writer (formatter ,format-in) ,@args)))

(defun-1 #_clojure.pprint/fresh-line ()
  (fresh-line))
