;;; NB This is in a separate file to avoid a circular dependency.
(defsystem "cloture-test"
  :defsystem-depends-on ("cloture")
  :depends-on ("fiveam")
  :perform (test-op (o c) (symbol-call :cloture.test :run-cloture-tests))
  :serial t
  :components ((:module "clojure"
                :components (("cloture:cljc" "test")))
               (:module "t"
                :components ((:file "test")
                             ("cloture:cljc" "cloture-tests")))))
