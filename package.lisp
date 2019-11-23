(defpackage #:cloture
  (:use
    ;; :cl with a portable MOP.
    #:closer-common-lisp
    ;; Utility libraries.
    #:alexandria #:serapeum
    ;; A namespace for readtables.
    #:named-readtables
    ;; Pattern matching.
    #:trivia)
  (:import-from #:trivia.ppcre #:ppcre)
  (:import-from #:cl-custom-hash-table
    #:define-custom-hash-table-constructor)
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
  (:shadow :@ :atom :true)
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

           #:truthy?
           #:falsy?
           #:clojure=

           #:defun-1
           #:expose-to-clojure
           #:defgeneric-1

           #:clojurize

           #:map->list #:list->map

           #:|#{}| #:{} #:[]
           #:autogensyms

           #:declare-keywords
           #:proclaim-keywords
           #:nil?
           #:not=

           #:repl))
