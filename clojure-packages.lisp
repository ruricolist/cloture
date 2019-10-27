(defpackage "clojure.core"
  (:nicknames "clj")
  (:use)
  (:import-from #:cl
    #:< #:>= #:<= #:+ #:- #:-
    #:quote)
  (:export . #.(append
                (serapeum:lines (alexandria:read-file-into-string (asdf:system-relative-pathname "cloture" "core-syms.txt")))
                '("&" "var"))))

(defpackage "user"
  (:use "clojure.core"))
