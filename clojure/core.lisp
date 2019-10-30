(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Note that there are many special cases here that could be compiled more efficiently or inlined, or macro-expanded more legibly. For now simplicity & uniformity is the goal. In the future, maybe, when there is more code to test against, optimization might be worthwile. Not yet.

(defconst special-forms
  '#_(quote
      if do def let binding var
      loop recur throw try))

(define-symbol-macro #_true t)
(define-symbol-macro #_false nil)
(define-symbol-macro #_nil nil)

(eval-always
  (defun parse-docs (body)
    (if (stringp (first body))
        (values (first body) (rest body))
        (values nil body)))

  (defun car+cdr (list)
    (values (car list) (cdr list))))

(defmacro defun-1 (name args &body body)
  "Define NAME in both the function and value namespaces.
That's defun-1 as in Lisp-1."
  `(progn
     (defun ,name ,args ,@body)
     (define-symbol-macro ,name #',name)
     ',name))

(defmacro defgeneric-1 (name args &body body)
  `(progn
     (defgeneric ,name ,args ,@body)
     (define-symbol-macro ,name #',name)
     ',name))

(defun-1 #_meta (x)
  (meta x))

(defun-1 #_zero? (n) (zerop n))
(defun-1 #_neg? (n) (minusp n))
(defun-1 #_pos? (n) (plusp n))

(defmacro #_quote (x)
  `(quote ,x))

(defmacro #_if (test then &optional (else #_nil))
  `(if (truthy? ,test) ,then ,else))

(defmacro #_if-not (test then &optional (else #_nil))
  `(if (falsy? ,test) ,then ,else))

(defmacro #_do (&rest exprs)
  `(progn ,@exprs))

;;; Here's how dynamic variables work. In Clojure `let' always
;;; produces lexical variables, only `binding' can rebind variables.
;;; So "dynamic" variables are defined as a symbol macro with a
;;; backing Lisp special (the "var"). Using `let' just rebinds the
;;; symbol macro. But `binding' expands the symbol macro.

(expose-to-clojure #_*out* *standard-output*)
(expose-to-clojure #_*err* *standard-error*)
(expose-to-clojure #_*in* *standard-input*)
(expose-to-clojure #_*ns* *package*)

(expose-to-clojure #_*1 *)
(expose-to-clojure #_*2 **)
(expose-to-clojure #_*3 ***)

(expose-to-clojure #_*print-length* *print-length*)
(expose-to-clojure #_*print-level* *print-level*)
(expose-to-clojure #_*print-readably* *print-readably*)
(expose-to-clojure #_*read-eval* *read-eval*)

(defconst clojure-var-prefix '*clojure-var-)

(defmacro #_def (name &body body)
  (mvlet* ((docstring expr
            (ematch body
              ((list (and docstring (type string))
                     expr)
               (values docstring expr))
              ((list expr)
               (values nil expr))))
           (dynamic? (meta-ref name :|dynamic|))
           (private? (meta-ref name :|private|))
           (meta-doc (meta-ref name :|doc|))
           (doc (or docstring meta-doc)))
    `(progn
       ,(if dynamic?
            (let ((backing-var (symbolicate clojure-var-prefix name)))
              `(progn
                 (expose-to-clojure ,name ,backing-var)
                 (defparameter ,backing-var ,expr
                   ,@(unsplice doc))))
            `(def ,name ,expr
               ,@(unsplice doc)))
       ,@(unsplice
          (unless private?
            `(export ',name)))
       ',name)))

(defmacro expose-to-clojure (clj cl)
  "Export a Lisp special variable as a Clojure dynamic binding."
  `(progn
     (eval-always
       (setf (meta-ref ',clj :|dynamic|) t))
     (define-symbol-macro ,clj ,cl)
     ',clj))

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
  "Clojure let.
Bear in mind Clojure let works like Lisp `let*' (bindings are
nested)."
  (let* ((bindings (convert 'list bindings)))
    `(clojure-let ,bindings
       ,@body)))

(defmacro #_binding (bindings &body body &environment env)
  (let* ((bindings (convert 'list bindings))
         (binds (batches bindings 2 :even t))
         (symbols (mapcar #'first binds))
         (exprs (mapcar #'second binds))
         (vars (mapcar (op (var _ env)) symbols)))
    ;; NB Clojure `binding' works in parallel (unlike Clojure `let').
    `(let ,(mapcar #'list vars exprs)
       ,@body)))

(defun var (sym &optional env)
  (let ((exp (macroexpand-1 (assure symbol sym) env)))
    (when (or (eql exp sym)
              (not (symbolp exp))
              (not (meta-ref sym :|dynamic|)))
      (error "Not a var: ~a" sym))
    exp))

(defmacro #_var (symbol &environment env)
  `(quote ,(var symbol env)))

(defmacro #_defn (name &body body)
  (mvlet ((docstring body (parse-docs body)))
    `(progn (defalias ,name
              (#_fn ,name ,@body)
              ,@(unsplice docstring))
            (#_def ,name #',name))))

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
               (return-from #_fn
                 `(#_fn (,params ,@exprs))))
              ((and clauses (type list))
               `(ematch ,args
                  ,@(collecting
                      (dolist (clause clauses)
                        (mvlet* ((params exprs (car+cdr clause))
                                 (pats rest all (dissect-seq-pattern params)))
                          (when (symbol-package all)
                            (error "No :as in fn."))
                          (collect
                              `(,(if rest
                                     `(list* ,@pats ,rest)
                                     `(list ,@pats))
                                ,@exprs))))))))))
    (if name
        `(named-lambda ,name (&rest ,args)
           ,@(unsplice docstr)
           ,expr)
        `(lambda (&rest ,args)
           ,@(unsplice docstr)
           ,expr))))

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

(defun-1 #_list (&rest items)
  items)

(defun-1 #_list* (&rest items)
  ;; TODO non-lists
  (cond ((null items))
        ((single items) (first items))
        (t
         (fset:reverse
          (fset:concat (fset:reverse items)
                       (reverse (butlast items)))))))

(defun-1 #_refer-clojure (&rest args)
  (apply #'#_refer args))

(defun-1 #_refer (ns &key exclude only rename)
  (let ((p (find-package ns)))
    (if (nor exclude only) (use-package p)
        (let* ((exports (package-exports p))
               (exports
                 (if only
                     (intersection only exports :test #'string=)
                     exports))
               (exports
                 (if exclude
                     (set-difference exports exclude :test #'string=)
                     exports)))
          (import exports)
          (when rename
            (do-map (from to rename)
              (let ((from (find-external-symbol from p :error t))
                    (to (intern (string to))))
                (eval `(progn
                         (defmacro to (&body body)
                           (cons ',from body))
                         (define-symbol-macro ,to ,from))))))))))

(defun-1 #_require (&rest args)
  "Implements Clojure require (and use, because)."
  (flet ((expect-package (lib)
           (loop
             (when (find-package lib)
               (return))
             (cerror "Try again"
                     "No such package as ~a"))))
    (dolist (arg args)
      (nlet rec ((prefix "")
                 (arg arg))
        (match arg
          ((and libspec (type symbol))
           (expect-package (string+ prefix libspec)))
          ((and libspec (type seq))
           (ematch (convert 'list libspec)
             ((lambda-list lib &key as refer exclude only rename)
              (when (and (or exclude only rename) refer)
                (error "Invalid: ~a" libspec))
              (let ((lib (string+ prefix lib)))
                (expect-package lib)
                (when as
                  (nick:add-package-local-nickname as lib))
                (when refer
                  (match refer
                    (:all (#_refer lib))
                    (otherwise
                     (#_refer lib :only (convert 'list refer)))))
                (when (or exclude only rename)
                  (#_refer lib :only only
                               :exclude exclude
                               :rename rename))))))
          ((list* prefix libspecs)
           (dolist (libspec libspecs)
             (rec prefix libspec))))))))

(defun-1 #_use (&rest args)
  (apply #'#_require args))

(defmacro #_ns (name &body refs)
  (mvlet ((name (string name))
          (docstr refs (parse-docs refs)))
    `(eval-always
       (defpackage ,(string name)
         ,@(unsplice docstr))
       (in-package ,(string name))
       ,@(unsplice
          (unless (find :|refer-clojure| refs :key #'car)
            `(use-package "clojure.core")))
       ,@(loop for ref in refs
               collect (ematch ref
                         ;; TODO
                         (())
                         ((list* :|gen-class| _))
                         ((list* :|import| _))
                         ((list* :|load| _))
                         ((list* :|use| args)
                          `(#_use ,@args))
                         ((list* :|require| args)
                          `(#_require ,@args))
                         ((list* :|refer-clojure| args)
                          `(#_refer-clojure ,@args))))
       (find-package ,name))))

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

(defun-1 #_apply (fn &rest args)
  (apply #'apply fn args))

(defun-1 #_str (&rest args)
  (apply #'string+ args))

(defun-1 #_throw (arg)
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

(defconstructor protocol
  (name symbol)
  (functions list))

(define-namespace protocol protocol)

(defmacro #_defprotocol (name &body specs)
  (mvlet* ((doc specs (parse-docs specs))
           (sigs
            (collecting
              (dolist (spec specs)
                (ematch spec
                  ((lambda-list name
                                (and args (type seq))
                                &optional docs)
                   (let ((lambda-list (seq->lambda-list args)))
                     (unless lambda-list
                       (error "Protocol function cannot be nullary."))
                     (collect (list name lambda-list docs)))))))))
    `(progn
       (eval-always
         (setf (symbol-protocol ',name)
               (protocol ',name ',sigs))
         ,@(unsplice
            (when doc
              `(setf (documentation ',name 'protocol)
                     ,doc))))
       ,@(loop for (name lambda-list docs) in sigs
               collect `(defgeneric-1 ,name ,lambda-list
                          ,@(unsplice (and docs `(:documentation ,docs)))))
       (#_def ,name (symbol-protocol ',name)))))

(defgeneric-1 #_extends? (protocol atype))
(defgeneric-1 #_satisfies? (protocol x))

(defun check-protocol (protocol-name fn-names)
  ;; TODO Are protocols supposed to be exhaustive?
  (let* ((protocol (symbol-protocol protocol-name))
         (protocol-functions (protocol-functions protocol)))
    (assert (subsetp fn-names protocol-functions))))

(defmacro #_extend-type (type &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (p . methods) in specs
               for fn-names = (mapcar #'first methods)
               do (check-protocol p fn-names)
               collect `(defmethods ,type
                            (:method #_extends? ((x ,type)) t)
                          ,@(loop for (fn-name arg-seq . body) in methods
                                  for lambda-list = (seq->lambda-list arg-seq)
                                  for this = (first lambda-list)
                                  collect `(:method ,fn-name ((,this ,type)
                                                              ,@(rest lambda-list))
                                             ,@body)))))))

(defmacro #_extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(#_extend-type ,type ,p ,@methods)))))

(defun split-specs (specs)
  "Split the common Clojure syntax of a symbol (protocol, type) and a list of protocol/interface implementations."
  (runs specs :test (lambda (x y) (declare (ignore x))
                      (not (symbolp y)))))

(defun-1 #_contains? (col x)
  (fset:contains? col x))

(defun-1 #_inc (n)
  (1+ n))

(defun-1 #_dec (n)
  (1- n))

(defun-1 #_* (&rest ns)
  (apply #'* ns))

(defun-1 #_/ (&rest ns)
  (apply #'/ ns))

(defmacro #_locking (x &body body)
  `(synchronized (,x)
     ,@body))

(defun seq-with (seq idx val)
  "Like `fset:with', but IDX must be less than the length of SEQ."
  (if (<= idx (size seq))
      (with seq idx val)
      (error "Idx ~a not valid for ~a" idx seq)))

(defun-1 #_assoc (map key val &rest kvs)
  (let ((with (if (typep map 'seq) #'seq-with
                  #'fset:with)))
    (reduce (lambda (map kv)
              (apply with map kv))
            (cons (list key val)
                  (batches kvs 2 :even t))
            :initial-value map)))

(defun-1 #_dissoc (map key &rest keys)
  (reduce #'less
          (cons key keys)
          :initial-value map))

(defun-1 #_alter-meta! (obj f &rest args)
  (synchronized (obj)
    (setf (meta obj)
          (apply f (meta obj) args))))

(defmacro #_cond (&rest clauses)
  (ematch clauses
    ((list) #_nil)
    ((list test expr)
     `(#_if ,test ,expr #_nil))
    ((list* test expr clauses)
     `(#_if ,test ,expr (#_cond ,@clauses)))))

(defgeneric-1 #_seq (coll)
  (:method ((list list))
    (or list #_nil))
  (:method ((seq sequence))
    (if (emptyp seq) #_nil seq))
  (:method ((seq seq))
    (if (empty? seq) #_nil
        seq))
  (:method ((map map))
    (if (empty? map) #_nil
        (collecting
          (do-map (k v map)
            (collect (seq k v))))))
  (:method ((set set))
    (if (empty? set) #_nil
        (collecting
          (do-set (x set)
            (collect x))))))

(defun-1 #_count (col)
  (size col))

(defmacro #_letfn (fnspecs &body body)
  (let* ((fnspecs (convert 'list fnspecs)))
    `(fbindrec ,(loop for (name . body) in fnspecs
                      collect `(,name (#_fn ,name ,@body)))
       ,@body)))

(defun-1 #_gensym (&optional (prefix-string "G__"))
  (gensym prefix-string))

(defun-1 #_get (map key &optional (not-found #_nil))
  (multiple-value-bind (val val?)
      (lookup map key)
    (if val? val not-found)))

(defun-1 #_constantly (x)
  (constantly x))

(defun-1 #_alter-var-root (root f &rest args)
  (synchronized (root)
    (setf (symbol-value root)
          (apply f (symbol-value root) args))))

(defvar *assert* t)
(expose-to-clojure #_*assert* *assert*)

(defmacro #_assert (test &optional message)
  `(when *assert*
     (assert ,test () ,@(unsplice message))))

(defun-1 #_special-symbol? (s)
  (true (memq s special-forms)))

(defun-1 #_unchecked-add-int (x y)
  (+ x y))

(defun-1 #_unchecked-multiply-int (x y)
  (- x y))

(defmacro #_declare (&rest args)
  `(progn
     ,@(loop for arg in args
             collect `(progn
                        (defalias ,arg (constantly #_nil))
                        (#_def arg #_nil)))))

(defun-1 #_hash (x)
  (murmurhash:murmurhash x))

(defun-1 #_= (&rest args)
  (match args
    ((list) t)
    ((list _) t)
    ((list x y) (equal? x y))
    (otherwise (every #'equal? args (rest args)))))

(defun-1 #_not= (&rest args)
  (not (apply #'#_= args)))

(defun-1 #_== (&rest args)
  (apply #'= args))

(defun-1 #_pop (seq)
  (etypecase seq
    (list (rest seq))
    (seq (fset:subseq seq 0 (1- (size seq))))))

(defmacro #_comment (&body body)
  (declare (ignore body))
  (values))
