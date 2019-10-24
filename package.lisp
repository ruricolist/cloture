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
    #:seq)
  (:shadow :@)
  (:shadowing-import-from #:fset #:map)
  (:export #:cloture #:meta-ref))
