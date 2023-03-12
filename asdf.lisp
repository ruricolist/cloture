(in-package #:cloture)

(defclass clj (asdf:cl-source-file)
  ((type :initform "clj")))

(defclass cljc (clj)
  ((type :initform "cljc")))

(defclass cljs (clj)
  ((type :initform "cljs")))

(defmethod asdf/lisp-action:call-with-around-compile-hook ((c clj) thunk)
  (let ((*package* (find-package "user")))
    (call-next-method)))

(defmethod asdf:perform :around ((o asdf:compile-op) (c clj))
  ;; This doesn't work; ASDF forcibly rebinds to cl-user.
  (let ((*package* (find-package "user")))
    (with-clojure-reader ()
      (call-next-method))))
