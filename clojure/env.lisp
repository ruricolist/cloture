(in-package #:cloture)

(define-symbol-macro %clojure-env
  #.(fset:with
     (empty-clojure-map)
     :lisp-env nil))

(eval-always
  (defun get-clojure-env (lisp-env)
    (fset:with (macroexpand-1 '%clojure-env lisp-env)
               :lisp-env lisp-env)))
