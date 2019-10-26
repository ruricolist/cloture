(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Note that there are many special cases here that could be compiled more efficiently or macro-expanded more legibly. For now simplicity & uniformity is the goal. In the future, maybe, when there is more code to test against, optimization might be worthwile. Not yet.

(define-symbol-macro #_true t)
(define-symbol-macro #_false nil)
(define-symbol-macro #_nil nil)

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

(defun1 #_meta (x)
  (meta x))

(defun1 #_zero? (n) (zerop n))
(defun1 #_neg? (n) (minusp n))
(defun1 #_pos? (n) (plusp n))

(defmacro #_quote (x)
  `(quote ,x))

(defmacro #_if (test then &optional (else #_nil))
  `(if (truthy? ,test) ,then ,else))

(defmacro #_if-not (test then &optional (else #_nil))
  `(if (falsy? ,test) ,then ,else))

(defmacro #_do (&rest exprs)
  `(progn ,@exprs))

(defmacro #_def (symbol &body body)
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

(defmacro #_defn (name &body body)
  (mvlet ((docstring body (parse-docs body)))
    `(progn (defalias ,name
              (#_fn ,name ,@body)
              ,@(unsplice docstring))
            (#_def ,name #',name))))

(defmacro clojure-let1 (pattern expr &body body)
  `(ematch ,expr
     (,pattern ,@body)))

(defmacro clojure-let (bindings &body body)
  (match bindings
    ((list) `(#_do ,@body))
    ((list* (and pattern (type symbol)) expr bindings)
     `(let ((,pattern ,expr))
        (clojure-let ,bindings
          ,@body)))
    ((list* pattern expr bindings)
     `(ematch ,expr
        (,(obj->pattern pattern)
         (clojure-let ,bindings
           ,@body))))))

(defmacro #_let (bindings &body body)
  (let* ((bindings (convert 'list bindings)))
    `(clojure-let ,bindings
       ,@body)))

(defmacro #_fn (&body body)
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

(defmacro #_var (symbol)
  `(quote ,symbol))

(defmacro #_loop (binds &body body)
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

(defmacro #_recur (&rest args)
  `(%recur (list ,@args)))

;; (defmacro #_try (&body body))

(defun1 #_list (&rest items)
  items)

(defun1 #_list* (&rest items)
  ;; TODO non-lists
  (cond ((null items))
        ((single items) (first items))
        (t
         (fset:reverse
          (fset:concat (fset:reverse items)
                       (reverse (butlast items)))))))

(defmacro #_ns (name &body refs)
  (mvlet ((docstr body (parse-docs refs)))
    (declare (ignore body))             ;TODO
    `(progn
       (defpackage ,(string name)
         (:use "clojure.core")
         ,@(unsplice docstr))
       (in-package ,(string name)))))

(defmacro #_in-ns (name)
  `(in-package ,(string name)))

(defmacro #_defmacro (name &body body)
  (mvlet ((docstr body (parse-docs body))
          (forms (string-gensym 'forms)))
    `(defmacro ,name (&whole #_&form
                      &body ,forms
                      &environment #_&env)
       ,@(unsplice docstr)
       (declare (ignorable #_&form #_&env))
       (apply (#_fn ,@body) ,forms))))

(defmacro #_and (&rest forms)
  (if (null forms) #_true
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (#_if-not ,val
                     ,val
                     (#_and ,@(rest forms)))))))

(defmacro #_or (&rest forms)
  (if (null forms) #_nil
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (#_if ,val ,val
                 (#_or ,@(rest forms)))))))

(defmacro #_when (test &body body)
  `(#_if ,test (#_do ,@body)))

(defun1 #_apply (fn &rest args)
  (apply #'apply fn args))

(defun1 #_str (&rest args)
  (apply #'string+ args))

(defun1 #_throw (arg)
  (error arg))

(defmacro #_-> (x &rest steps)
  (reduce (lambda (x step)
            (list* (first step)
                   x
                   (rest step)))
          steps
          :key #'ensure-list
          :initial-value x))

(defmacro #_->> (x &rest steps)
  (reduce (flip #'append1)
          steps
          :key #'ensure-list
          :initial-value x))

(defun1 #_contains? (col x)
  (fset:contains? col x))

(defun1 #_inc (n)
  (1+ n))

(defun1 #_dec (n)
  (1- n))

(defun1 #_* (&rest ns)
  (apply #'* ns))

(defun1 #_/ (&rest ns)
  (apply #'/ ns))
