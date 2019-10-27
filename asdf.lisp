(in-package #:cloture)

(defclass clj-file (asdf:cl-source-file)
  ((type :initform "clj")))

(defclass cljc-file (clj-file)
  ((type :initform "cljc")))

(defclass cljs-file (clj-file)
  ((type :initform "cljs")))

(defmethod asdf:perform :around ((o asdf:compile-op) (c clj-file))
  (with-clojure-reader ()
    (call-next-method)))
