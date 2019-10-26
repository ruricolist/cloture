(defpackage #:cloture
  (:use #:cl #:alexandria #:serapeum #:named-readtables #:trivia)
  (:import-from #:fset
    #:lookup
    #:less
    #:do-map
    #:do-seq
    #:with
    #:empty-map
    #:convert
    #:seq
    #:size)
  (:shadow :@)
  (:shadowing-import-from #:fset #:map)
  (:export #:cloture                    ;Readtable.
           #:meta-ref
           #:read-clojure
           #:read-clojure-from-string
           #:slurp-clojure-stream
           #:slurp-clojure-file
           #:load-clojure
           #:compile-clojure
           #:load-compile-clojure))
