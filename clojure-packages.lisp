(defpackage "clojure.core"
  (:nicknames "clj")
  (:use)
  (:import-from #:cl
    #:< #:>= #:<= #:+ #:- #:-
    #:quote)
  (:export . #.(append
                (serapeum:lines
                 (alexandria:read-file-into-string
                  (asdf:system-relative-pathname "cloture" "core-syms.txt")))
                '("&" "var" "true" "false" "nil"
                  "quote" "if" "do" "def" "recur" "throw" "try"
                  "invoke" "equiv"
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
                  "Object"))))

(defpackage "clojure.pprint"
  (:use)
  (:export . #.(serapeum:lines
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname "cloture" "pprint-syms.txt")))))

(defpackage "clojure.string"
  (:use)
  (:export "starts-with?" "ends-with?"))

(defpackage "clojure.test"
  (:use)
  (:export #:is #:thrown? #:thrown-with-msg #:testing
           #:with-test #:deftest #:run-tests #:run-all-tests
           #:*load-tests* #:use-fixtures))

(defpackage "clojure.template"
  (:use)
  (:export "apply-template" "do-template"))

(defpackage "user"
  (:use "clojure.core"))

(defpackage #:cloture.impl
  (:documentation "Package used for the Clojure shortcut reader macro.")
  (:use
    "clojure.core"
    "clojure.pprint"
    "clojure.string"
    "clojure.test"
    "clojure.template"))
