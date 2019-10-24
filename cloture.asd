(defsystem "cloture"
  :depends-on ("serapeum"
               "cl-arrows"
               "fset"
               "named-readtables"
               "fare-quasiquote-readtable")
  :in-order-to ((test-op (test-op "cloture/test")))
  :serial t
  :components ((:file "package")
               (:file "clojure-packages")
               (:file "cloture")
               (:file "readtable")
               (:module "clojure"
                :components ((:file "core")))))

(defsystem "cloture/test"
  :depends-on ("cloture" "fiveam")
  :perform (test-op (o c) (symbol-call :cloture.test :run-cloture-tests))
  :serial t
  :components ((:module "t"
                :components ((:file "test")))))
