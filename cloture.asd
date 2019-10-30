(defsystem "cloture"
  :depends-on ("serapeum"
               "cl-arrows"
               "fset"
               "named-readtables"
               "fare-quasiquote-readtable"
               "trivial-package-local-nicknames"
               "cl-murmurhash"
               "lisp-namespace"
               "cl-ppcre")
  :in-order-to ((test-op (test-op "cloture/test")))
  :serial t
  :components ((:file "package")
               (:file "fset-hacks")
               (:static-file "core-syms.txt")
               (:file "clojure-packages")
               (:file "cloture")
               (:file "readtable")
               (:file "asdf")
               (:module "clojure"
                :components ((:file "core")))))

(defsystem "cloture/test"
  :depends-on ("cloture" "fiveam")
  :perform (test-op (o c) (symbol-call :cloture.test :run-cloture-tests))
  :serial t
  :components ((:module "t"
                :components ((:file "test")))))
