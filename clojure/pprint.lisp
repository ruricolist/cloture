(in-package #:cloture)
(in-readtable clojure-shortcut)

(expose-to-clojure
  #_*print-base*             *print-base*
  #_*print-miser-width*      *print-miser-width*
  #_*print-pprint-dispatch*  *print-pprint-dispatch*
  #_*print-pretty*           *print-pretty*
  #_*print-radix*            *print-radix*
  #_*print-right-margin*     *print-right-margin*)

(defun-1 #_cl-format (writer format-in &rest args)
  (format writer "~?" format-in args))

(defmacro #_formatter (format-in)
  `(formatter ,format-in))

(define-compiler-macro #_cl-format (&whole call writer format-in &rest args)
  (if (stringp format-in) call
      `(#_cl-format ,writer (formatter ,format-in) ,@args)))

(defun-1 #_fresh-line ()
  (fresh-line))
