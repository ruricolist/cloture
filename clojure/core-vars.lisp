(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Defining these variables means calling `meta` at compile time, which requires that Clojure maps be available, which requires that Clojure `=` and `hash` be available.

(expose-to-clojure
  #_*out* *standard-output*
  #_*err* *standard-error*
  #_*in* *standard-input*
  #_*ns* *package*)

(expose-to-clojure
 #_*1 *
 #_*2 **
 #_*3 ***)

(expose-to-clojure
  #_*print-length* *print-length*
  #_*print-level* *print-level*
  #_*print-readably* *print-readably*
  #_*read-eval* *read-eval*)

(expose-to-clojure #_*unchecked-math* *unchecked-math*)

(expose-to-clojure #_*assert* *assert*)
