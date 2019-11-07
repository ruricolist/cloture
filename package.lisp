(defpackage #:cloture
  (:use #:cl #:alexandria #:serapeum #:named-readtables #:trivia)
  (:local-nicknames (#:nick #:trivial-package-local-nicknames))
  (:import-from #:trivia.ppcre #:ppcre)
  (:import-from #:fset
    #:equal?
    #:empty?
    #:lookup
    #:less
    #:do-map
    #:do-seq
    #:do-set
    #:with
    #:empty-map
    #:convert
    #:seq
    #:size)
  (:import-from #:lisp-namespace
    #:define-namespace)
  (:shadowing-import-from #:fset
    #:map
    #:set)
  (:shadow :@)
  (:export #:cloture                    ;Readtable.
           #:meta-ref
           #:read-clojure
           #:read-clojure-from-string
           #:slurp-clojure-stream
           #:slurp-clojure-file
           #:load-clojure
           #:compile-clojure
           #:load-compile-clojure
           #:clojure-shortcut
           #:clj
           #:cljc
           #:cljs

           #:defun-1
           #:expose-to-clojure
           #:defgeneric-1

           #:clojurize

           #:map->list #:list->map

           #:%set #:%map #:%seq))
