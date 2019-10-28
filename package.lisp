(defpackage #:cloture
  (:use #:cl #:alexandria #:serapeum #:named-readtables #:trivia)
  (:import-from #:fset
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
           #:cljs))
