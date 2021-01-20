(defsystem "cloture"
  :description "Clojure in Common Lisp"
  :license "EPL-1.0"
  :depends-on ("serapeum"
               "trivia.ppcre"
               "arrows"
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
               "global-vars"
               "esrap"
               "iterate"
               "sycamore")
  :in-order-to ((test-op (test-op "cloture-test")))
  :serial t
  :components ((:file "package")
               (:static-file "core-syms.txt")
               (:static-file "pprint-syms.txt")
               (:file "clojure-packages")
               (:file "iterate-drivers")
               (:file "fset-hacks")
               (:file "cloture")
               (:file "quasiquote")
               (:file "interpol")
               (:file "readtable")
               (:file "walk")
               (:file "asdf")
               (:file "clojure-printer")
               (:file "errors")
               (:module "clojure"
                :serial t
                :components ((:file "core")
                             (:file "sycamore")
                             (:file "core2")
                             (:file "pprint")
                             (:file "template")
                             (:file "set")
                             (:file "string")
                             (:file "stacktrace")
                             (:file "walk")
                             (:file "cloture")))
               (:file "clojure-repl")))
