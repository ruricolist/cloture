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
                '("&" "var"))))

(defpackage "clojure.pprint"
  (:use "clojure.core")
  (:export . #.(serapeum:lines
                (alexandria:read-file-into-string
                 (asdf:system-relative-pathname "cloture" "pprint-syms.txt")))))

(defpackage "user"
  (:use "clojure.core"))
