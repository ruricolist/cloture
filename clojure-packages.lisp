(defpackage "clojure.core"
  (:nicknames "clj")
  (:use)
  (:import-from #:cl
    #:< #:>= #:<= #:+ #:- #:-
    #:quote)
  (:export
   #:> #:< #:>= #:<= #:+ #:- #:-
   #:& #:->
   ;; Special forms.
   "def" "if" "do" "let" "quote" "var" "fn"
   "loop" "recur" "throw" "try"
   "monitor-enter" "monitor-exit"
   ;; "Java" interop.
   "set!" "new" "."
   ;; Needed for "throw".
   "catch" "finally"
   "true" "false" "nil"
   "def" "defn"
   "fn"

   "loop" "recur"
   "throw"

   "cond" "cond->"

   "merge"

   "list" "list*"))

(defpackage "user"
  (:use "clojure.core"))
