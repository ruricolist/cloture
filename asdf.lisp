(in-package #:cloture)

(defclass clj (asdf:cl-source-file)
  ((type :initform "clj")))

(defclass cljc (clj)
  ((type :initform "cljc")))

(defclass cljs (clj)
  ((type :initform "cljs")))

(defmethod asdf:perform :around ((o asdf:compile-op) (c clj))
  (with-clojure-reader ()
    (call-next-method)))
