(in-package #:cloture)
(in-readtable :standard)

(define-symbol-macro |clojure.core|:|true| t)
(define-symbol-macro |clojure.core|:|false| nil)
(define-symbol-macro |clojure.core|:|nil| nil)

(defmacro defun1 (name args &body body)
  "Define NAME in both the function and value namespaces."
  `(progn
     (defun ,name ,args ,@body)
     (def ,name #',name)
     ',name))

(defmacro |clojure.core|:|quote| (x)
  `(quote ,x))

(defmacro |clojure.core|:|if| (test then &optional (else |clojure.core|:|nil|))
  `(if (truthy? ,test) ,then ,else))

(defmacro |clojure.core|:|do| (&rest exprs)
  `(progn ,@exprs))

(defmacro |clojure.core|:|def| (symbol &body body)
  (mvlet* ((docstring expr
            (ematch body
              ((list (and docstring (type string))
                     expr)
               (values docstring expr))
              ((list expr)
               (values nil expr))))
           (dynamic? (meta-ref symbol :|dynamic|))
           (private? (meta-ref symbol :|private|))
           (meta-doc (meta-ref symbol :|doc|))
           (doc (or docstring meta-doc)))
    `(progn
       ,(if dynamic?
            `(defparameter ,symbol ,expr
               ,@(unsplice doc))
            `(def ,symbol ,expr
               ,@(unsplice doc)))
       ,@(unless private?
           (require-body-for-splice
            `((export ',symbol))))
       ',symbol)))

(defmacro |clojure.core|:|defn| (name &body body)
  `(progn (defun ,name (&rest args)
            (apply (fn ,@body) args))
          (def ,name #',name)))

(defmacro |clojure.core|:|let| (bindings &body body)
  ;; TODO destructuring
  (let* ((bindings (convert 'list bindings))
         (binds (batches bindings 2 :even t))
         (patterns (mapcar (op (obj->pattern (first _))) binds))
         (exprs (mapcar #'second binds)))
    ;; TODO with-read-only-vars?
    `(multiple-value-ematch
         (values ,@exprs)
       (,patterns
        ,@body))))

(defmacro |clojure.core|:|fn| ())

(defmacro |clojure.core|:|var| (symbol)
  `(quote ,symbol))

(defmacro |clojure.core|:|loop| (binds &body body)
  `(|clojure.core|:|let| ,binds
                  (nlet ((%recur (data)))
                      ,@body)))

(defmacro |clojure.core|:|recur| ())

(defmacro |clojure.core|:|throw| (expr)
  `(error ,expr))

;; (defmacro |clojure.core|:|try| (&body body))

(defun1 |clojure.core|:|list| (&rest items)
  items)

(defun1 |clojure.core|:|list*| (&rest items)
  ;; TODO non-lists
  (apply #'list* items))
