(defpackage #:cloture
  (:use
    ;; :cl with a portable MOP.
    #:closer-common-lisp
    ;; Utility libraries.
    #:alexandria #:serapeum
    ;; A namespace for readtables.
    #:named-readtables
    ;; Pattern matching.
    #:trivia
    #:murmurhash
    #:iterate)
  (:import-from #:trivia.ppcre #:ppcre)
  (:shadowing-import-from #:iterate
    #:sum #:summing #:in)
  (:shadowing-import-from #:serapeum
    #:collecting)
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
    #:empty-seq
    #:empty-set
    #:empty-map
    #:convert
    #:seq
    #:size)
  (:shadowing-import-from #:fset #:with)
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
           #:compile-load-clojure
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

           #:repl

           #:regex

           ;; Iterate drivers.
           #:in-seq
           #:on-seq
           #:in-indexed))
