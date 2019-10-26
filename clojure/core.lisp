(in-package #:cloture)
(in-readtable :standard)

(define-symbol-macro |clojure.core|:|true| t)
(define-symbol-macro |clojure.core|:|false| nil)
(define-symbol-macro |clojure.core|:|nil| nil)

(eval-always
  (defun parse-docs (body)
    (if (stringp (first body))
        (values (first body) (rest body))
        (values nil body))))

(defmacro defun1 (name args &body body)
  "Define NAME in both the function and value namespaces."
  `(progn
     (defun ,name ,args ,@body)
     (define-symbol-macro ,name #',name)
     ',name))

(defun1 |clojure.core|:|meta| (x)
  (meta x))

(defun1 |clojure.core|:|zero?| (n) (zerop n))
(defun1 |clojure.core|:|neg?| (n) (minusp n))
(defun1 |clojure.core|:|pos?| (n) (plusp n))

(defmacro |clojure.core|:|quote| (x)
  `(quote ,x))

(defmacro |clojure.core|:|if| (test then &optional (else |clojure.core|:|nil|))
  `(if (truthy? ,test) ,then ,else))

(defmacro |clojure.core|:|if-not| (test then &optional (else |clojure.core|:|nil|))
  `(if (falsy? ,test) ,then ,else))

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
  (mvlet ((docstring body (parse-docs body)))
    `(progn (defalias ,name
              (|clojure.core|:|fn| ,name ,@body)
              ,@(unsplice docstring))
            (|clojure.core|:|def| ,name #',name))))

(defmacro clojure-let1 (pattern expr &body body)
  `(ematch ,expr
     (,pattern ,@body)))

(defmacro clojure-let (bindings &body body)
  (match bindings
    ((list) `(|clojure.core|:|do| ,@body))
    ((list* (and pattern (type symbol)) expr bindings)
     `(let ((,pattern ,expr))
        (clojure-let ,bindings
          ,@body)))
    ((list* pattern expr bindings)
     `(ematch ,expr
        (,(obj->pattern pattern)
         (clojure-let ,bindings
           ,@body))))))

(defmacro |clojure.core|:|let| (bindings &body body)
  (let* ((bindings (convert 'list bindings)))
    `(clojure-let ,bindings
       ,@body)))

(defmacro |clojure.core|:|fn| (&body body)
  (mvlet* ((docstr body (parse-docs body))
           (name
            (and (symbolp (car body))
                 (pop body)))
           (args (string-gensym 'args))
           (expr
            (ematch body
              ((list* (and params (type seq))
                      exprs)
               `(clojure-let (,params ,args)
                  ,@exprs))
              ((and clauses (type list))
               `(ematch ,args
                  ,@(loop for (params . exprs) in clauses
                          collect `(,(obj->pattern params)
                                    ,@exprs)))))))
    (if name
        `(named-lambda ,name (&rest ,args)
           ,@(unsplice docstr)
           ,expr)
        `(lambda (&rest ,args)
           ,@(unsplice docstr)
           ,expr))))

(defmacro |clojure.core|:|var| (symbol)
  `(quote ,symbol))

(defmacro |clojure.core|:|loop| (binds &body body)
  (let* ((binds (convert 'list binds))
         (binds (batches binds 2 :even t))
         (exprs (mapcar #'second binds))
         (pats (mapcar (compose #'obj->pattern #'first) binds)))
    (with-unique-names (args)
      `(nlet %recur ((,args (list ,@exprs)))
         (ematch ,args
           ((list ,@pats)
            ,@body))))))

(defmacro %recur (&rest args)
  (declare (ignore args))
  (error "Cannot use `recur' outside `loop'."))

(defmacro |clojure.core|:|recur| (&rest args)
  `(%recur (list ,@args)))

;; (defmacro |clojure.core|:|try| (&body body))

(defun1 |clojure.core|:|list| (&rest items)
  items)

(defun1 |clojure.core|:|list*| (&rest items)
  ;; TODO non-lists
  (cond ((null items))
        ((single items) (first items))
        (t
         (fset:reverse
          (fset:concat (fset:reverse items)
                       (reverse (butlast items)))))))

(defmacro |clojure.core|:|ns| (name &body refs)
  (mvlet ((docstr body (parse-docs refs)))
    (declare (ignore body))             ;TODO
    `(progn
       (defpackage ,(string name)
         (:use "clojure.core")
         ,@(unsplice docstr))
       (in-package ,(string name)))))

(defmacro |clojure.core|:|in-ns| (name)
  `(in-package ,(string name)))

(defmacro |clojure.core|:|defmacro| (name &body body)
  (mvlet ((docstr body (parse-docs body))
          (forms (string-gensym 'forms)))
    `(defmacro ,name (&whole |clojure.core|:|&form|
                      &body ,forms
                      &environment |clojure.core|:|&env|)
       ,@(unsplice docstr)
       (declare (ignorable |clojure.core|:|&form| |clojure.core|:|&env|))
       (apply (|clojure.core|:|fn| ,@body) ,forms))))

(defmacro |clojure.core|:|and| (&rest forms)
  (if (null forms)
      |clojure.core|:|true|
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (|clojure.core|:|if-not| ,val
                          ,val
                          (|clojure.core|:|and| ,@(rest forms)))))))

(defmacro |clojure.core|:|or| (&rest forms)
  (if (null forms)
      |clojure.core|:|nil|
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (|clojure.core|:|if| ,val
                          ,val
                          (|clojure.core|:|or| ,@(rest forms)))))))

(defmacro |clojure.core|:|when| (test &body body)
  `(|clojure.core|:|if| ,test (|clojure.core|:|do| ,@body)))

(defun1 |clojure.core|:|apply| (fn &rest args)
  (apply #'apply fn args))

(defun1 |clojure.core|:|str| (&rest args)
  (apply #'string+ args))

(defun1 |clojure.core|:|throw| (arg)
  (error arg))

(defmacro |clojure.core|:-> (x &rest steps)
  (reduce (lambda (x step)
            (list* (first step)
                   x
                   (rest step)))
          steps
          :key #'ensure-list
          :initial-value x))

(defmacro |clojure.core|:->> (x &rest steps)
  (reduce (flip #'append1)
          steps
          :key #'ensure-list
          :initial-value x))

(defun1 |clojure.core|:|contains?| (col x)
  (fset:contains? col x))

(defun1 |clojure.core|:|inc| (n)
  (1+ n))

(defun1 |clojure.core|:|dec| (n)
  (1- n))

(defun1 |clojure.core|:* (&rest ns)
  (apply #'* ns))

(defun1 |clojure.core|:/ (&rest ns)
  (apply #'/ ns))
