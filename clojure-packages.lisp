(in-package #:cloture)

(eval-always
  (defvar *clojure-packages*
    (tg:make-weak-hash-table :weakness :key))

  (defun mark-clojure-package (package)
    (let ((package (find-package package)))
      (synchronized ('*clojure-packages*)
        (setf (href *clojure-packages* package) t))))

  (defun clojure-package? (package)
    (href *clojure-packages* (find-package package)))

  (defun clojure-symbol? (symbol)
    (clojure-package? (symbol-package symbol))))

(defmacro declaim-clojure-packages (&rest packages)
  `(eval-always
     (mapc #'mark-clojure-package ',packages)))

(defmacro define-clojure-package (name &body body)
  `(progn
     ;; Use define-package instead of defpackage so SBCL doesn't
     ;; complain about package variance.
     (uiop:define-package ,name ,@body)
     (declaim-clojure-packages ,name)))

(define-clojure-package :clojure-classes
  (:use))

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
                  "def-"
                  "&env" "&form"
                  "invoke" "equiv" "lookup"
                  "contains-key?" "kv-reduce"
                  "new"
                  "Fn"
                  "set!"
                  "imul"
                  "IAssociative"
                  "ICollection" "-conj"
                  "IComparable"
                  "ICounted"
                  "IDeref"
                  "IEditableCollection" "as-transient"
                  "IEmptyableCollection"
                  "IEquiv"
                  "IFn"
                  "IHash"
                  "IIndexable"
                  "IIndexed"
                  "IIterable"
                  "IKVReduce"
                  "ILookup"
                  "IMap" "-dissoc"
                  "IMeta"
                  "INext"
                  "IPending"
                  "IReduce" "internal-reduce"
                  "IReversible"
                  "ISeq"
                  "ISeqable"
                  "ISequential"
                  "ISorted"
                  "IStack"
                  "IVector" "assoc-n"
                  "IWithMeta"
                  "Object" "toString" ".toString"
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
                  "IllegalAccessError" "IllegalAccessError."
                  ".getClassName"
                  "Integer/MAX_VALUE"
                  "Integer/MIN_VALUE"
                  "Long/MIN_VALUE"
                  "Long/MAX_VALUE"
                  "atom?"
                  ;; These aren't part of Clojure but the REPL needs
                  ;; them (and Leiningen provides them).
                  "exit" "quit"))))

(define-clojure-package "clojure.pprint"
  (:use)
  (:export . #.(serapeum:lines
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname "cloture" "pprint-syms.txt")))))

(define-clojure-package "clojure.set"
  (:use)
  (:export "select" "union" "difference" "intersection" "subset?" "superset?"))

(define-clojure-package "clojure.string"
  (:use)
  (:export "starts-with?" "ends-with?" "join" "replace" "re-quote-replacement" "upper-case"))

(define-clojure-package "clojure.template"
  (:use)
  (:export "apply-template" "do-template"))

(define-clojure-package "clojure.stacktrace"
  (:use)
  (:export "print-cause-trace" "print-stack-trace" "print-throwable" "root-cause"))

(define-clojure-package "clojure.walk"
  (:use)
  (:export "postwalk" "postwalk-demo" "postwalk-replace"))

(define-clojure-package "user"
  (:use "clojure.core"))

(define-clojure-package #:cloture.impl
  (:documentation "Package used for the Clojure shortcut reader macro.")
  (:use
    "clojure.core"
    "clojure.pprint"
    "clojure.set"
    "clojure.string"
    "clojure.template"
    "clojure.stacktrace"
    "clojure.walk")
  (:shadowing-import-from "clojure.core" "replace"))
