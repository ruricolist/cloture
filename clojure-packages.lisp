(defpackage "clojure.core"
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

(defpackage "clojure.pprint"
  (:use)
  (:export . #.(serapeum:lines
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname "cloture" "pprint-syms.txt")))))

(defpackage "clojure.string"
  (:use)
  (:export "starts-with?" "ends-with?"))

(defpackage "clojure.template"
  (:use)
  (:export "apply-template" "do-template"))

(defpackage "clojure.stacktrace"
  (:use)
  (:export "print-cause-trace" "print-stack-trace" "print-throwable" "root-cause"))

(defpackage "user"
  (:use "clojure.core"))

(defpackage #:cloture.impl
  (:documentation "Package used for the Clojure shortcut reader macro.")
  (:use
    "clojure.core"
    "clojure.pprint"
    "clojure.string"
    "clojure.template"
    "clojure.stacktrace"))
