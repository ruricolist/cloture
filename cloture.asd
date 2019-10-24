(defsystem "cloture"
  :depends-on ("serapeum"
               "cl-arrows"
               "fset"
               "named-readtables"
               "fare-quasiquote-readtable")
  :serial t
  :components ((:file "package")
               (:file "packages")
               (:file "cloture")
               (:file "readtable")
               (:module "clojure"
                :components ((:file "core")))))
