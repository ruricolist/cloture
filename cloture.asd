(defsystem "cloture"
  :description "Clojure in Common Lisp"
  :license "EPL-1.0"
  :depends-on ("serapeum"
               "trivia.ppcre"
               "cl-arrows"
               "fset"
               "named-readtables"
               "fare-utils"
               "cl-murmurhash"
               "lisp-namespace"
               "cl-ppcre"
               "fiveam"
               "closer-mop"
               "cl-custom-hash-table"
               "stmx"
               "atomics"
               "lparallel"
               "overlord"
               "cl-interpol"
               "global-vars")
  :in-order-to ((test-op (test-op "cloture-test")))
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
                             (:file "set")
                             (:file "string")
                             (:file "stacktrace")))
               (:file "repl")))
