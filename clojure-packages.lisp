(in-package #:cloture)

(eval-always
  (defvar *clojure-packages*
    (tg:make-weak-hash-table :weakness :key))

  (defun mark-clojure-package (package)
    (let ((package (find-package package)))
      (synchronized ('*clojure-packages*)
        (setf (href *clojure-packages* package) t))))

  (defun clojure-package? (package)
    (href *clojure-packages* (find-package package))))

(defmacro declaim-clojure-packages (&rest packages)
  `(eval-always
     (mapc #'mark-clojure-package ',packages)))

(defmacro define-clojure-package (name &body body)
  `(progn
     (uiop:define-package ,name ,@body)
     (declaim-clojure-packages ,name)))

(define-clojure-package "clojure.core"
  (:nicknames "clj")
  (:use)
  (:export . #.(append
                (serapeum:lines
                 (alexandria:read-file-into-string
                  (asdf:system-relative-pathname "cloture" "core-syms.txt")))
                ;; Some CL stuff it is nice to have available.
                lambda-list-keywords
                '("&" "var" "true" "false" "nil"
                  "quote" "if" "do" "def" "recur" "throw" "try" "catch" "finally"
                  "&env" "&form"
                  "invoke" "equiv" "lookup"
                  "Throwable"
                  "IAssociative"
                  "ICollection"
                  "IComparable"
                  "ICounted"
                  "IEmptyableCollection"
                  "IEquiv"
                  "IFn"
                  "IHash"
                  "IIndexable"
                  "IIndexed"
                  "IIterable"
                  "IKVReduce"
                  "ILookup"
                  "IMap"
                  "IMeta"
                  "INext"
                  "IReduce"
                  "IReversible"
                  "ISeq"
                  "ISeqable"
                  "ISequential"
                  "ISorted"
                  "IStack"
                  "IWithMeta"
                  "Object"
                  "Thread" "Thread."
                  "Throwable" "Throwable."
                  ".getMessage" ".getCause"
                  ".getStackTrace" ".printStackTrace"
                  "Exception" "Exception."
                  "Error" "Error."
                  "RuntimeException" "RuntimeException."
                  "IllegalArgumentException" "IllegalArgumentException."
                  "IllegalStateException" "IllegalStateException."
                  "AssertionError" "AssertionError."
                  "ArityException" "ArityException."
                  ".getClassName"
                  ;; These aren't part of Clojure but the REPL needs
                  ;; them (and Leiningen provides them).
                  "exit" "quit"))))

(define-clojure-package "clojure.pprint"
  (:use)
  (:export . #.(serapeum:lines
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname "cloture" "pprint-syms.txt")))))

(define-clojure-package "clojure.string"
  (:use)
  (:export "starts-with?" "ends-with?"))

(define-clojure-package "clojure.template"
  (:use)
  (:export "apply-template" "do-template"))

(define-clojure-package "clojure.stacktrace"
  (:use)
  (:export "print-cause-trace" "print-stack-trace" "print-throwable" "root-cause"))

(define-clojure-package "user"
  (:use "clojure.core"))

(define-clojure-package #:cloture.impl
  (:documentation "Package used for the Clojure shortcut reader macro.")
  (:use
    "clojure.core"
    "clojure.pprint"
    "clojure.string"
    "clojure.template"
    "clojure.stacktrace"))
