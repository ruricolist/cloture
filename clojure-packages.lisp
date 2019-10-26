(defpackage "clojure.core"
  (:nicknames "clj")
  (:use)
  (:import-from #:cl
    #:< #:>= #:<= #:+ #:- #:-
    #:quote)
  (:export
   #:> #:< #:>= #:<= #:+ #:- #:- #:* #:/
   #:&
   #:-> #:->>

   ;; Special forms.
   "def" "if" "do" "let" "quote" "var" "fn"
   "loop" "recur" "throw" "try"
   "monitor-enter" "monitor-exit"
   ;; "Java" interop.
   "set!" "new" "."
   ;; Needed for "throw".
   "catch" "finally"

   "if-not"
   "true" "false" "nil"
   "def" "defn"
   "fn"

   "loop" "recur"
   "throw"

   "cond" "cond->"

   "merge"

   "list" "list*"

   "ns" "in-ns"

   "and" "or" "when"

   "defmacro" "&env" "&form"

   "meta"

   "zero?" "neg?" "pos?"
   "dec" "inc"

   "apply" "str"

   "contains?"


   ))

(defpackage "user"
  (:use "clojure.core"))
