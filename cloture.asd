(defsystem "cloture"
  :depends-on ("serapeum"
               "trivia.ppcre"
               "cl-arrows"
               "fset"
               "named-readtables"
               "fare-utils"
               "trivial-package-local-nicknames"
               "cl-murmurhash"
               "lisp-namespace"
               "cl-ppcre"
               "fiveam"
               "closer-mop"
               "cl-custom-hash-table"
               "stmx"
               "atomics"
               "lparallel"
               "overlord")
  :in-order-to ((test-op (test-op "cloture/test")))
  :serial t
  :components ((:file "package")
               (:static-file "core-syms.txt")
               (:static-file "pprint-syms.txt")
               (:file "clojure-packages")
               (:file "fset-hacks")
               (:file "cloture")
               (:file "quasiquote")
               (:file "readtable")
               (:file "asdf")
               (:file "errors")
               (:module "clojure"
                :serial t
                :components ((:file "core")
                             (:file "pprint")
                             (:file "template")
                             (:file "strings")
                             (:file "stacktrace")))
               (:file "repl")))

(defsystem "cloture/test"
  :depends-on ("cloture" "fiveam")
  :perform (test-op (o c) (symbol-call :cloture.test :run-cloture-tests))
  :serial t
  :components ((:module "t"
                :components ((:file "test")))))
