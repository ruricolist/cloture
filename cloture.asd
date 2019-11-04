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
               "fiveam")
  :in-order-to ((test-op (test-op "cloture/test")))
  :serial t
  :components ((:file "package")
               (:file "fset-hacks")
               (:static-file "core-syms.txt")
               (:static-file "pprint-syms.txt")
               (:file "clojure-packages")
               (:file "errors")
               (:file "cloture")
               (:file "quasiquote")
               (:file "readtable")
               (:file "asdf")
               (:module "clojure"
                :components ((:file "core")
                             (:file "pprint")
                             (:file "template")))))

(defsystem "cloture/test"
  :depends-on ("cloture" "fiveam")
  :perform (test-op (o c) (symbol-call :cloture.test :run-cloture-tests))
  :serial t
  :components ((:module "t"
                :components ((:file "test")))))
