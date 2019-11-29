(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Note that there are many special cases here that could be compiled more efficiently or inlined, or macro-expanded more legibly. For now simplicity & uniformity is the goal. In the future, maybe, when there is more code to test against, optimization might be worthwile. Not yet.

;;; TODO make Lisp lambda lists and Clojure arg vectors congruent when possible.

;;; Not inlining these would be criminal.
(declaim (inline . #_(bit-and bit-or bit-xor bit-not bit-flip bit-set bit-shift-right bit-shift-left bit-and-not bit-clear bit-test unsigned-bit-shift-right)))

(declaim (inline . #_(cons < > >= <= + - / * mod rem)))

(defconst empty-list '())

(defmacro defun-1 (name args &body body)
  "Define NAME in both the function and value namespaces.
That's defun-1 as in Lisp-1.

Implicitly downcases the keywords for keyword arguments. Also, if no
default argument is given for keywords or optional arguments, the
default default is Clojure's nil instead of Lisp nil."
  (mvlet* ((required optional rest keywords aok aux key?
            (parse-ordinary-lambda-list args))
           (keywords
            (loop for ((keyword-name name) init supplied?)
                    in keywords
                  collect `((,(make-keyword (string-invert-case keyword-name))
                             ,name)
                            ,(or init #_nil) ,supplied?)))
           (optional
            (loop for (name init suppliedp) in optional
                  collect `(,name ,(or init #_nil) ,suppliedp)))
           (args
            (unparse-ordinary-lambda-list
             required optional rest keywords aok aux key?))
           (predicate? (string$= "?" name))
           (body (if predicate?
                     `((assure clojure-boolean
                         ,@body))
                     body)))
    `(progn
       (defun ,name ,args ,@body)
       (define-symbol-macro ,name #',name)
       ',name)))

(defmacro defalias-1 (name fn)
  "Like `defalias', but in both namespaces.
For the rare case where the identity of the function matters (e.g.
defmulti)."
  `(progn
     (defalias ,name ,fn)
     (define-symbol-macro ,name #',name)
     ',name))

(defmacro defgeneric-1 (name args &body body)
  `(progn
     (defgeneric ,name ,args ,@body)
     (define-symbol-macro ,name #',name)
     ',name))

(-> ? (t) clojure-boolean)
(defun ? (x)
  "Convert a (generalized) Lisp boolean to a Clojure boolean."
  (if x #_true #_false))

(defun-1 #_not (x)
  (? (falsy? x)))

(defun nil? (x)
  (eql x #_nil))

(defun-1 #_nil? (x)
  (? (eql x #_nil)))

(defun-1 #_false? (x)
  (? (eql x #_false)))

(defun-1 #_true? (x)
  (? (eql x #_true)))

(defun-1 #_identical? (x y)
  (? (eq x y)))

(defun-1 #_zero? (n) (? (zerop n)))
(defun-1 #_neg? (n) (? (minusp n)))
(defun-1 #_pos? (n) (? (plusp n)))
(defun-1 #_odd? (n) (? (oddp n)))
(defun-1 #_even? (n) (? (evenp n)))

(defmacro #_quote (x)
  `(quote ,(clojurize x)))

(defun-1 #_eval (x)
  (eval (declojurize x)))

(defmacro define-clojure-macro (name args &body body)
  (flet ((split-args-on (args kw)
           (if-let (tail (member kw args))
             (values (second tail)
                     (append (ldiff args tail)
                             (cddr tail)))
             (values nil args))))
    (mvlet* ((body docs (body+docs+attrs body))
             (body decls (parse-body body))
             (whole-decls decls
              (partition-declarations '(#_&form) decls))
             (env-decls decls
              (partition-declarations '(#_&env) decls))
             (forms (string-gensym 'forms))
             (env args (split-args-on args '&environment))
             (whole args (split-args-on args '&whole))
             (rest-or-body (if (memq '&body args) '&body '&rest))
             )
      `(locally
           ;; Suppress SBCL warnings about &form and &env being suspicious variables.
           (declare #+sbcl (sb-ext:muffle-conditions style-warning))
         (defmacro ,name (,@(and whole `(&whole ,whole))
                          ,rest-or-body ,forms
                          ,@(and env `(&environment ,env)))
           ,@(unsplice docs)
           ,@env-decls
           (declojurize
            (block ,name              ;catch return-from
              (let ((,forms (clojurize ,forms))
                    ,@(unsplice (and whole `(,whole (clojurize ,whole)))))
                ,@whole-decls
                (destructuring-bind ,args ,forms
                  ,@decls
                  (declare #+sbcl (sb-ext:unmuffle-conditions style-warning))
                  ,@body)))))))))

(defun-1 #_macroexpand-1 (form)
  (clojurize (clojure-macroexpand-1 form)))

(defun-1 #_macroexpand (form)
  (loop for f = form then exp
        for exp = (#_macroexpand-1 f)
        until (eq f exp)
        finally (return f)))

(define-clojure-macro #_if (test then &optional (else #_nil))
  `(if (truthy? ,test) ,then ,else))

(define-clojure-macro #_if-not (test then &optional (else #_nil))
  `(#_if ,test ,else ,then))

(define-clojure-macro #_do (&body exprs)
  `(progn ,@exprs))

;;; Here's how dynamic variables work. In Clojure `let' always
;;; produces lexical variables, only `binding' can rebind variables.
;;; So "dynamic" variables are defined as a symbol macro with a
;;; backing Lisp special (the "var"). Using `let' just rebinds the
;;; symbol macro. But `binding' expands the symbol macro.

(defconst clojure-var-prefix '*clojure-var-)

(defun backing-var (symbol)
  (assert (not (string^= clojure-var-prefix symbol)))
  (symbolicate clojure-var-prefix symbol))

(define-clojure-macro #_def- (name &body body)
  "Define a (private) var."
  (mvlet* ((docstring expr
            (ematch body
              ((list (and docstring (type string))
                     expr)
               (values docstring expr))
              ((list expr)
               (values nil expr))))
           ;; TODO How does the reader magically know to assign
           ;; :dynamic to the var, and not the symbol, or does the
           ;; meta get moved?
           (dynamic? (truthy? (meta-ref name :|dynamic|)))
           (meta-doc (meta-ref name :|doc|))
           (doc (or docstring
                    (and (stringp meta-doc)
                         meta-doc)))
           (backing-var (backing-var name)))
    `(progn
       ,(if dynamic?
            `(progn
               (expose-to-clojure ,name ,backing-var)
               (defparameter ,backing-var ,expr
                 ,@(unsplice doc)))
            `(progn
               ;; NB The symbol macro must comes first so EXPR can
               ;; close over it.
               (define-symbol-macro ,name ,backing-var)
               (global-vars:define-global-parameter* ,backing-var ,expr
                 ,@(unsplice doc))))
       (eval-always
         (setf (meta-ref ',backing-var :|ns|) ,(symbol-package name)
               (meta-ref ',backing-var :|name|) ',name
               (meta-ref ',backing-var :|dynamic|) ,(? dynamic?)))
       (defun ,name (&rest args)
         (ifn-apply ,name args))
       ',name)))

(defmacro export-unless-private (name)
  (if (meta-ref name :|private|)
      `(eval-always
         (setf (meta-ref ',(backing-var name) :|private|) #_true))
      `(export ',name ,(package-name (symbol-package name)))))

(define-clojure-macro #_def (name &body body)
  `(progn
     (#_def- ,name ,@body)
     (export-unless-private ,name)
     ',name))

(defun-1 #_ns-interns (ns)
  (let ((map (empty-map))
        (ns (#_the-ns ns)))
    (do-symbols (s ns map)
      (when (eql (symbol-package s) ns)
        (when-let (var (find-var s))
          (withf map s var))))))

(defmacro expose-to-clojure-1 (clj cl)
  "Export a Lisp special variable as a Clojure dynamic binding."
  `(progn
     (eval-always
       (setf (meta-ref ',clj :|dynamic|) t))
     (define-symbol-macro ,clj ,cl)
     ',clj))

(defmacro expose-to-clojure (&body pairs)
  (let ((pairs (batches pairs 2 :even t)))
    `(progn
       ,@(loop for (clj cl) in pairs
               collect `(expose-to-clojure-1 ,clj ,cl)))))

(expose-to-clojure
  #_*out* *standard-output*
  #_*err* *standard-error*
  #_*in* *standard-input*
  #_*ns* *package*)

(expose-to-clojure
  #_*1 *
  #_*2 **
  #_*3 ***)

(expose-to-clojure
  #_*print-length* *print-length*
  #_*print-level* *print-level*
  #_*print-readably* *print-readably*
  #_*read-eval* *read-eval*)

(defmacro with-syms-fbound (syms &body body)
  (assert (every #'symbolp syms))
  (assert (notany #'keywordp syms))
  (let ((syms (remove '_ syms :test #'string=)))
    (with-unique-names (args)
      `(macrolet ,(loop for sym in syms
                        collect `(,sym (&rest ,args)
                                       (list* 'ifncall ',sym ,args)))
         ,@body))))()

(define-clojure-macro clojure-let (bindings &body body)
  (match bindings
    ((list) `(#_do ,@body))
    ((list* (and pattern (type symbol)) expr bindings)
     `(let ((,pattern ,expr))
        (with-syms-fbound (,pattern)
          (clojure-let ,bindings
            ,@body))))
    ((list* pattern expr bindings)
     ;; TODO bind in function namespace
     (multiple-value-bind (pattern syms)
         (obj->pattern pattern)
       `(ematch ,expr
          (,pattern
           (with-syms-fbound ,syms
             (clojure-let ,bindings
               ,@body))))))))

(define-clojure-macro #_let (bindings &body body)
  "Clojure let.
Bear in mind Clojure let works like Lisp `let*' (bindings are
nested)."
  (let* ((bindings (convert 'list (assure seq bindings))))
    `(clojure-let ,bindings
       ,@body)))

(define-clojure-macro #_binding (bindings &body body &environment env)
  (let* ((bindings (convert 'list bindings))
         (binds (batches bindings 2 :even t))
         (symbols (mapcar #'first binds))
         (exprs (mapcar #'second binds))
         (vars (mapcar (op (var _ env)) symbols)))
    ;; NB Clojure `binding' works in parallel (unlike Clojure `let').
    `(let ,(mapcar #'list vars exprs)
       ,@body)))

(define-clojure-macro #_var (symbol &environment env)
  `(quote ,(var symbol env)))

(defun-1 #_find-var (symbol)
  (or (find-var symbol) #_nil))

(defun-1 #_var-get (var)
  (symbol-value var))

(define-clojure-macro #_defn (name &body body)
  (mvlet ((body docs (body+docs+attrs body)))
    `(#_def ,name ,@(unsplice docs)
            (#_fn ,name ,@body))))

(define-clojure-macro #_defn- (name &body body)
  (mvlet ((body docs (body+docs+attrs body)))
    `(#_def- ,name ,@(unsplice docs)
             (#_fn ,name ,@body))))

(define-clojure-macro #_fn (&body body*)
  (local
    (def (values body docstr) (body+docs+attrs body*))
    (def name
      (and (symbolp (car body))
           (pop body)))
    (def args-sym (string-gensym 'args))
    (def clauses
      (ematch body
        ((list* (and _ (type seq)) _)
         (list (parse-clause body)))
        ((and clauses (type list))
         (mapcar #'parse-clause clauses))))
    (when (no clauses)
      (error "No clauses in fn body ~s" body*))
    (def variadic-clauses (filter #'fn-clause-rest clauses))

    (when (length> variadic-clauses 1)
      (error (clojure-syntax-error "Only one variadic overload in an fn.")))

    (def variadic (first variadic-clauses))

    (def sorted-clauses
      (concatenate 'list
                   (sort-new (remove variadic clauses) #'< :key #'fn-clause-min-arity)
                   (and variadic (list variadic))))

    (defun fn-clause->trivia-clause (clause)
      (mvlet* ((params (fn-clause-params clause))
               (lambda-list (seq->lambda-list params :allow-patterns t))
               (syms (nth-value 1 (obj->pattern params)))
               (body (fn-clause->body clause)))
        `((lambda-list ,@lambda-list)
          (with-syms-fbound ,syms
            ,@body))))

    (def dispatch
      `(nlet %recur ((,args-sym ,args-sym))
         (match ,args-sym
           ,@(mapcar #'fn-clause->trivia-clause sorted-clauses)
           (otherwise (error (#_ArityException. (length ,args-sym) ',name))))))
    (if name
        `(named-lambda ,name (&rest ,args-sym)
           ,@(unsplice docstr)
           ,dispatch)
        `(lambda (&rest ,args-sym)
           ,@(unsplice docstr)
           ,dispatch))))

(define-clojure-macro #_loop (binds &body body)
  (mvlet* ((binds (convert 'list binds))
           (specs (batches binds 2 :even t))
           (exprs (mapcar #'second specs))
           (binds (convert 'seq (mapcar #'first specs))))
    `(funcall (#_fn ,binds ,@body)
              ,@exprs)))

(defmacro %recur (&rest args)
  (declare (ignore args))
  (error (clojure-syntax-error "Cannot use `recur' outside `loop'.")))

(define-clojure-macro #_recur (&rest args)
  `(%recur (list ,@args)))

;; (defmacro #_try (&body body))

(defmacro alias-from (from to)
  `(progn
     (defmacro ,to (&body body)
       (cons ',from body))
     (define-symbol-macro ,to ,from)))

(defun qualify-symbol (ns symbol)
  (intern (string+ ns "/" symbol)))

(defun unqualify-symbol (symbol)
  (nth-value 1 (ns+name symbol)))

(defun-1 #_refer-clojure (&rest args)
  (apply #'#_refer :|clojure.core| args))

(defun setup-qualified-names (p &optional prefix)
  (let* ((package (find-package p))
         (prefix (or prefix (package-name package))))
    (dolist (export (package-exports package))
      (let ((qname (qualify-symbol prefix export)))
        (eval `(alias-from ,export ,qname))))))

(defun-1 #_refer (ns &key (exclude nil) (only nil) (rename nil))
  (let ((p (find-package ns)))
    (setup-qualified-names p)
    (if (and exclude only) (use-package p)
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
                (eval `(alias-from ,from ,to)))))))))

(defun-1 #_require (&rest args)
  "Implements Clojure require (and use, because)."
  (flet ((expect-ns (lib)
           (loop
             (when (find-package lib)
               (return))
             (cerror "Try again"
                     "No such namespace as ~a" lib))))
    (dolist (arg args)
      (nlet rec ((prefix "")
                 (arg arg))
        (match arg
          ((and libspec (type symbol))
           (let ((name (string+ prefix libspec)))
             (expect-ns name)
             (setup-qualified-names name)))
          ((and libspec (type seq))
           (ematch (convert 'list libspec)
             ((lambda-list lib &key |as| |refer| |exclude| |only| |rename|)
              (when (and (or |exclude| |only| |rename|) |refer|)
                (error (clojure-error "Invalid: ~a" libspec)))
              (let ((lib (string+ prefix lib)))
                (expect-ns lib)
                (setup-qualified-names lib |as|)
                (when |refer|
                  (match |refer|
                    (:|all| (#_refer lib))
                    (otherwise
                     (#_refer lib :|only| (convert 'list |refer|)))))
                (when (or |exclude| |only| |rename|)
                  (#_refer lib
                           :|only| |only|
                           :|exclude| |exclude|
                           :|rename| |rename|))))))
          ((list* prefix libspecs)
           (dolist (libspec libspecs)
             (rec prefix libspec))))))))

(defun-1 #_use (&rest args)
  (apply #'#_require args))

(define-clojure-macro #_ns (name &body refs)
  (mvlet ((name (string name))
          (refs docstr (body+docs+attrs refs)))
    `(eval-always
       ;; Use define-package instead of defpackage so SBCL complain
       ;; about package variance.
       (define-clojure-package ,(string name)
         (:use)
         ,@(unsplice (and docstr `(:documentation ,docstr))))
       (in-package ,(string name))
       ,@(unsplice
          (unless (find :|refer-clojure| refs :key #'car)
            `(#_refer :|clojure.core|)))
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
                          `(apply #_require (#_quote ,args)))
                         ((list* :|refer-clojure| args)
                          `(apply #_refer-clojure (#_quote ,args)))))
       (find-package ,name))))

(define-clojure-macro #_in-ns (name)
  `(in-package ,(string name)))

(define-clojure-macro #_defmacro (name &body body)
  (mvlet ((body docstr (body+docs+attrs body))
          (forms (string-gensym 'forms)))
    `(#_do
      (define-clojure-macro ,name (&whole #_&form
                                          &body ,forms
                                          &environment #_&env)
        ,@(unsplice docstr)
        (declare (ignorable #_&form #_&env))
        (declojurize
         (apply (#_fn ,@body)
                (clojurize ,forms))))
      (export-unless-private ,name)
      ',name)))

(comment
  (|clojure.core|:|defmacro| foo () 'foo))

(define-clojure-macro #_and (&rest forms)
  (if (null forms) #_true
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (#_if-not ,val
                     ,val
                     (#_and ,@(rest forms)))))))

(define-clojure-macro #_or (&rest forms)
  (if (null forms) #_nil
      (with-unique-names (val)
        `(let ((,val ,(first forms)))
           (#_if ,val ,val
                 (#_or ,@(rest forms)))))))

(define-clojure-macro #_when (test &body body)
  `(#_if ,test (#_do ,@body)))

(define-clojure-macro #_when-not (test &body body)
  `(#_if-not ,test (#_do ,@body)))

(defun-1 #_apply (fn &rest args)
  (apply #'ifn-apply fn args))

(defun-1 #_throw (arg)
  (error arg))

(def #_Throwable (find-class 'condition))

(define-clojure-macro #_-> (x &rest steps)
  (reduce (lambda (x step)
            (list* (first step)
                   x
                   (rest step)))
          steps
          :key #'ensure-list
          :initial-value x))

(define-clojure-macro #_->> (x &rest steps)
  (reduce (flip #'append1)
          steps
          :key #'ensure-list
          :initial-value x))

(eval-always
  (defun gfname (name lambda-list)
    (intern (string+ name "/" (length lambda-list))
            (symbol-package name)))

  (defun check-protocol-lambda-list (lambda-list)
    (unless lambda-list
      (error (clojure-syntax-error "Protocol function cannot be nullary.")))
    (when (intersection lambda-list lambda-list-keywords)
      (error (clojure-syntax-error "Protocol functions cannot destructure.")))))

(defmacro defprotocol (name &body specs)
  (flet ((expand-multiple-arities (sigs)
           (let* ((lambda-lists (mapcar #'second sigs)))
             (mapc #'check-protocol-lambda-list lambda-lists)
             (mvlet* ((docs (some #'third sigs))
                      (args (string-gensym 'args))
                      (gfnames
                       (mapcar (op (apply #'gfname (firstn 2 _))) sigs)))
               `(progn
                  ;; TODO Compiler macro for compile-time dispatching.
                  (defun-1 ,(caar sigs) (&rest ,args)
                    ,@(unsplice docs)
                    (ematch ,args
                      ,@(loop for lambda-list across (sort-new lambda-lists #'< :key #'length)
                              for gfname in gfnames
                              collect `((list ,@lambda-list)
                                        (,gfname ,@lambda-list)))))
                  ,@(loop for gfname in gfnames
                          for lambda-list in lambda-lists
                          collect `(defgeneric ,gfname ,lambda-list)))))))
    (mvlet ((sigs doc (body+docs+attrs specs)))
      `(progn
         (eval-always
           (setf (symbol-protocol ',name)
                 (protocol ',name ',sigs))
           ,@(unsplice
              (when doc
                `(setf (documentation ',name 'protocol)
                       ,doc))))
         ,@(let ((sigs (assort sigs :key #'car)))
             (loop for fn-arities in sigs
                   collect (expand-multiple-arities fn-arities)))
         (define-symbol-macro ,name (symbol-protocol ',name))))))

(define-clojure-macro #_defprotocol (name &body specs)
  `(defprotocol ,name ,@specs))

(defprotocol #_Object
  (#_toString (o)))

(defprotocol #_IEquiv
  (#_equiv (self other)))

(defprotocol #_ICollection
  (#_conj (coll x)))

(defun conj (coll x)
  (#_conj coll x))

(defprotocol #_ISeq
  (#_first (seq))
  (#_rest (seq)))

(defun first+rest (seq)
  (values (#_first seq)
          (#_rest seq)))

(defun-1 #_seq? (x)
  (#_satisfies? '#_ISeq x))

(defun seq? (x)
  (truthy? (#_seq x)))

(defun-1 #_second (x) (#_first (#_next x)))
(defun-1 #_fnext (x) (#_first (#_next x)))
(defun-1 #_ffirst (x) (#_first (#_first x)))
(defun-1 #_nfirst (x) (#_next (#_first x)))
(defun-1 #_nnext (x) (#_next (#_next x)))

(defprotocol #_INext
  (#_next (seq)))

(defprotocol #_IIndexed
  (#_nth (seq n))
  (#_nth (seq n not-found)))

(defun indexed? (x)
  (satisfies? '#_IIndexed x))

(deftype indexed ()
  '(satisfies indexed?))

(defprotocol #_Fn)

(defun-1 #_fn? (x)
  (#_satisfies? '#_Fn x))

(defprotocol #_IFn
  (#_invoke (x))
  (#_invoke (x y))
  (#_invoke (x y z))
  (#_invoke (x y z a))
  (#_invoke (x y z a b))
  (#_invoke (x y z a b c))
  (#_invoke (x y z a b c d))
  (#_invoke (x y z a b c d e))
  (#_invoke (x y z a b c d e f))
  (#_invoke (x y z a b c d e f g))
  (#_invoke (x y z a b c d e f g h))
  (#_invoke (x y z a b c d e f g h i))
  (#_invoke (x y z a b c d e f g h i j))
  (#_invoke (x y z a b c d e f g h i j k))
  (#_invoke (x y z a b c d e f g h i j k l))
  (#_invoke (x y z a b c d e f g h i j k l m))
  (#_invoke (x y z a b c d e f g h i j k l m n)))

(defun ifncall (ifn &rest args)
  ;; TODO avoid consing.
  (apply (ifn-function ifn) args))

(defun ifn-apply (ifn &rest args)
  (let* ((last (lastcar args))
         (args
           (if (listp last) args
               (append1 (butlast args)
                        (convert 'list last)))))
    (apply #'apply
           (ifn-function ifn)
           (convert 'list args))))

(-> ifn-function (t) (values function &optional))
(defun ifn-function (ifn)
  (cond ((functionp ifn) ifn)
        ((keywordp ifn)
         (lambda (map)
           (#_lookup map ifn)))
        ((satisfies? '#_IFn ifn)
         (lambda (&rest args)
           (apply #'#_invoke ifn args)))
        (t (error 'does-not-extend
                  :protocol '#_IFn
                  :object ifn))))

(defun-1 #_ifn? (x)
  (#_satisfies? '#_IFn x))

(defprotocol #_ISeqable
  (#_seq (x)))

(defprotocol #_IEmptyableCollection
  (#_empty (coll) "Create an empty collection."))

(defprotocol #_IHash
  (#_hash (x)))

(defprotocol #_ILookup
  (#_lookup (obj key))
  (#_lookup (obj key not-found)))

(defun lookupable? (x)
  (satisfies? '#_ILookup x))

(deftype lookupable ()
  '(satisfies lookupable?))

(defprotocol #_ICounted
  (#_count (col)))

(defprotocol #_IReversible
  (#_rseq (col)))

(defprotocol #_ISequential)

(defun-1 #_sequential? (x)
  (#_satisfies? '#_ISequential x))

(defprotocol #_IAssociative
  (#_contains-key? (coll k))
  (#_assoc (coll k v)))

(defun-1 dissoc (coll k &rest ks)
  (#_-dissoc coll k ks))

(defprotocol #_IMap
  (#_-dissoc (coll k ks)))

(defprotocol #_IKVReduce
  (#_kv-reduce (coll f init)))

(defprotocol #_IComparable
  (#_compare (x y)))

(defprotocol #_IMeta
  (#_meta (x)))

(defprotocol #_IWithMeta
  (#_with-meta (o meta)))

(defprotocol #_IStack
  (#_peek (s))
  (#_pop (s)))

(defprotocol #_IPending
  (#_realized? (x)))

(defprotocol #_IDeref
  (#_deref (x)))

(defprotocol #_IReduce
  (#_internal-reduce (coll f))
  (#_internal-reduce (coll f start)))

(defprotocol #_IEditableCollection
  (#_as-transient (coll)))

(defprotocol #_IVector
  (#_assoc-n (coll n val)))

(defgeneric-1 #_extends? (protocol atype)
  (:method ((protocol protocol) atype)
    (#_satisfies? (protocol-name protocol)
                  (class-prototype atype))))
(defgeneric-1 #_satisfies? (protocol x)
  (:method (name type)
    (declare (ignore name type))
    #_false)
  (:method ((protocol protocol) x)
    (#_satisfies? (protocol-name protocol) x)))

(defun satisfies? (protocol x)
  (truthy? (#_satisfies? protocol x)))

(defmacro extend-type (type &body specs)
  (labels ((expand-single-arity (spec)
             (mvlet* ((fn-name lambda-list body
                       (values (first spec) (second spec) (cddr spec)))
                      (this (first lambda-list)))
               (check-protocol-lambda-list lambda-list)
               `(defmethod ,fn-name ((,this ,type)
                                     ,@(rest lambda-list))
                  ,@body)))
           (expand-multiple-arities (arities)
             (loop for (fn-name lambda-list . body) in arities
                   for gfname = (gfname fn-name lambda-list)
                   do (check-protocol-lambda-list lambda-list)
                   collect (expand-single-arity (list* gfname lambda-list body)))))
    (let ((specs (split-specs specs)))
      `(progn
         ,@(loop for (p . methods) in specs
                 do (check-protocol p methods)
                 collect `(progn
                            (defmethod #_satisfies? ((protocol (eql ',p)) (x ,type))
                              #_true)
                            ,@(let ((arities (assort methods :key #'car)))
                                (loop for group in arities
                                      append (expand-multiple-arities group)))))))))

(define-clojure-macro #_extend-type (type &body specs)
  `(extend-type ,type ,@specs))

(defmacro extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(extend-type ,type ,p ,@methods)))))

(define-clojure-macro #_extend-protocol (p &body specs)
  `(extend-protocol ,p ,@specs))

;;; TODO A metaclass?

(defclass clojure-class ()
  ())

(define-clojure-macro #_deftype (type fields &body opts+specs)
  (mvlet* ((fields (convert 'list fields))
           (opts specs (parse-leading-keywords opts+specs))
           (specs (split-specs specs))
           (constructor-name
            (intern (string+ type ".")
                    (symbol-package type))))
    (declare (ignore opts))
    `(progn
       (defclass ,type (clojure-class)
         (,@(loop for field in fields
                  collect `(,field :initarg ,field))))
       (define-symbol-macro ,type (find-class ',type))
       ,@(loop for field in fields
               collect `(defun-1 ,(symbolicate ".-" field) (x)
                          (slot-value x ',field)))
       (defun-1 ,constructor-name ,fields
         (make-instance ',type
                        ,@(loop for field in fields
                                append `(',field ,field))))
       (defmethod print-object ((self ,type) stream)
         (print-unreadable-object (self stream :type t)
           (with-slots ,fields self
             (format stream "~{~a~^ ~}"
                     (list ,@fields)))))
       ;; TODO Should fields really shadow arguments?
       ,@(loop for (protocol-name . methods) in specs
               do (check-protocol protocol-name methods)
               append (loop for (fn-name arg-seq . body) in methods
                            for lambda-list = (seq->lambda-list arg-seq)
                            for gfname = (gfname fn-name lambda-list)
                            for this = (first lambda-list)
                            collect `(defmethod ,gfname ((,this ,type) ,@(rest lambda-list))
                                       (with-slots ,fields ,this
                                         ,@body)))))))

(extend-type t
  #_Object
  (#_toString (x) (princ-to-string x))
  #_IEquiv
  (#_equiv (x y) (? (equal? x y)))
  #_IComparable
  (#_compare
   (x y)
   (ecase (fset:compare x y)
     (:greater 1)
     (:less -1)
     (:equal 0)))
  #_INext
  (#_next (x) (#_seq (#_rest x))))

(extend-type package
  #_IEquiv
  (#_equiv (x y) (? (eql x y))))

(defun-1 #_str (&rest args)
  (with-output-to-string (s)
    (dolist (arg args)
      (unless (nil? arg)
        (write-string (#_toString arg) s)))))

(extend-type function
  #_Fn
  #_IFn
  (#_invoke (fn x) (ifn-apply fn x))
  (#_invoke (fn x y) (ifn-apply fn x y))
  (#_invoke (fn x y z) (ifn-apply fn x y z)))

(extend-protocol #_IHash
  t
  (#_hash (x) (murmurhash x)))

(extend-protocol #_ILookup
  map
  (#_lookup (map key)
            (#_lookup map key #_nil))
  (#_lookup (map key not-found)
            (multiple-value-bind (val val?)
                (fset:lookup map key)
              (if val? val not-found)))
  hash-table
  (#_lookup (table key)
            (gethash table key #_nil))
  (#_lookup (table key not-found)
            (gethash table key not-found))
  set
  (#_lookup (set key)
            (#_lookup set key #_nil))
  (#_lookup (set key not-found)
            (if (fset:lookup set key) key not-found)))

(extend-protocol #_IMeta
  t
  (#_meta (x) (or (meta x) #_nil)))

(extend-protocol #_IWithMeta
  t
  (#_with-meta (o meta) (with-meta o meta)))

(extend-protocol #_ICounted
  #_nil (#_count (x) 0)
  sequence (#_count (x) (length x))
  seq (#_count (x) (size x))
  map (#_count (x) (size x))
  hash-table (#_count (x) (hash-table-count x)))

(defun-1 #_counted? (x)
  (#_satisfies? '#_ICounted x))

(extend-type #_nil
  #_ISeq
  (#_first (x) #_nil)
  (#_rest (x) nil)
  #_ISeqable
  (#_seq (x) #_nil)
  #_IEmptyableCollection
  (#_empty (x) #_nil)
  #_ICollection
  (#_conj (coll x) (list x))
  #_ILookup
  (#_lookup (coll key) (declare (ignore key)) #_nil)
  (#_lookup (coll key default) (declare (ignore key)) default)
  #_IAssociative
  (#_contains-key? (coll k)
                   (declare (ignore k))
                   #_false)
  (#_assoc (coll k v) (fset:map (k v)))
  #_IEquiv
  (#_equiv (self other) (#_nil? other))
  #_IReduce
  (#_internal-reduce (coll f) (ifncall f))
  (#_internal-reduce (coll f start) (declare (ignore f)) start))

;;; Lisp null, Clojure's empty list.
(extend-type null
  #_ISeq
  (#_first (x) #_nil)
  (#_rest (x) nil)
  #_ISeqable
  (#_seq (x) #_nil)
  #_IEmptyableCollection
  (#_empty (x) nil)
  #_ICollection
  (#_conj (coll x) (list x))
  #_IStack
  (#_peek (c) #_nil)
  (#_pop (c) (error (#_IllegalStateException. "Pop on empty seq!")))
  #_ILookup
  (#_lookup (coll key default) (declare (ignore key)) default)
  (#_lookup (coll key) (declare (ignore key)) #_nil)
  #_IEquiv
  (#_equiv (n x) (or (null x) (#_empty? x)))
  #_IReduce
  (#_internal-reduce (coll f start) (declare (ignore f)) start)
  (#_internal-reduce (coll f) (ifncall f)))

(extend-type cons
  #_ISeq
  (#_first (x) (car x))
  (#_rest (x) (cdr x))
  #_ISeqable
  (#_seq (x) x)
  #_IEmptyableCollection
  (#_empty (x) nil)
  #_ICollection
  (#_conj (coll x) (cons x coll))
  #_IStack
  (#_peek (c) (car c))
  (#_pop (c) (cdr c))
  #_IEquiv
  (#_equiv (self other)
           (if (equal self other)
               #_true
               (if (seq? other)
                   (#_and
                    (#_equiv (car self) (#_first other))
                    (#_equiv (cdr self) (#_rest other)))
                   #_false)))
  #_ILookup
  (#_lookup (coll key)
            (#_lookup coll key #_nil))
  (#_lookup (coll key default)
            (nlet rec ((tail coll)
                       (i key))
              (if (not (plusp i))
                  (if tail (first tail)
                      default)
                  (rec (rest tail)
                       (1- i)))))
  #_IReduce
  (#_internal-reduce (coll f) (reduce-rests coll f nil nil))
  (#_internal-reduce (coll f start) (reduce-rests coll f start t)))

;; A Lisp vector (or a string).
(extend-type vector
  #_ISeq
  (#_first (x) (if (emptyp x) #_nil (aref x 0)))
  (#_rest (x) (if (emptyp x) () (nsubseq x 1)))
  #_ISeqable
  (#_seq (x) (if (emptyp x) #_nil x))
  #_IEmptyableCollection
  (#_empty (x) #())
  #_IReversible
  (#_rseq (x) (reverse x))
  #_IIndexed
  (#_nth (v n) (#_nth v n #_nil))
  (#_nth (v n not-found)
         (if (>= n (length v))
             not-found
             (aref v n)))
  #_ILookup
  (#_lookup (coll key) (#_nth coll key #_nil))
  (#_lookup (coll key default) (#_nth coll key default))
  #_IEquiv
  (#_equiv (self other)
           (if (and (vectorp other)
                    (vector= self other :test #'#_=))
               #_true
               (if (emptyp self)
                   (#_empty? other)
                   (#_and (#_equiv (#_first self)
                                   (#_first other))
                          (#_equiv (#_rest self)
                                   (#_rest other)))))))

(extend-type sequence
  #_ISeq
  (#_first (x) (if (emptyp x) #_nil (elt x 0)))
  (#_rest (x) (if (emptyp x) () (subseq x 1)))
  #_ISeqable
  (#_seq (seq) (if (emptyp seq) #_nil seq))
  #_IEmptyableCollection
  (#_empty (x) (subseq x 0 0))
  #_IFn
  (#_invoke (x arg) (elt x arg))
  #_IReversible
  (#_rseq (x) (reverse x))
  #_IIndexed
  (#_nth (v n) (#_nth v n #_nil))
  (#_nth (v n not-found)
         (if (>= n (length v))
             not-found
             (elt v n)))
  #_ILookup
  (#_lookup (coll key) (#_nth coll key #_nil))
  (#_lookup (coll key default) (#_nth coll key default))
  #_IReduce
  (#_internal-reduce (coll f) (reduce f coll))
  (#_internal-reduce (coll f start) (reduce f coll :initial-value start)))

(extend-type string
  #_IEquiv
  (#_equiv (x y)
           (? (and (stringp y)
                   (string= x y)))))

(extend-type fset:seq
  #_IVector
  (#_assoc-n (coll val n)
             (if (< n (size coll))
                 (with coll n val)
                 (error (clojure-program-error "Bad index for ~a" coll))))
  #_ISeq
  (#_first (x) (if (empty? x) #_nil (fset:first x)))
  (#_rest (x) (if (empty? x) () (fset:subseq x 1)))
  #_ISeqable
  (#_seq (seq) (if (empty? seq) #_nil seq))
  #_IEmptyableCollection
  (#_empty (seq) (fset:empty-seq))
  #_ICollection
  (#_conj (seq x) (fset:with-last seq x))
  #_IFn
  (#_invoke (x args) (fset:lookup x (only-elt args)))
  #_IReversible
  (#_rseq (x) (fset:reverse x))
  #_IIndexed
  (#_nth (v n) (#_nth v n #_nil))
  (#_nth (v n not-found)
         (if (>= n (size v))
             not-found
             (fset:lookup v n)))
  #_IStack
  (#_peek (c) (if (empty? c) #_nil
                  (lookup c (1- (size c)))))
  (#_pop (c) (if (empty? c) (error (#_IllegalStateException. "Empty seq"))
                 (fset:subseq c 0 (1- (size c)))))
  #_IAssociative
  (#_contains-key? (seq idx) (? (< -1 idx (size seq))))
  (#_assoc (seq idx value)
           (if (numberp idx)
               (#_assoc-n seq idx value)))
  #_IKVReduce
  (#_kv-reduce (seq f init)
               (if (empty? seq) seq
                   (let ((k 0))
                     (do-seq (v seq)
                       (setf init (ifncall f init (finc k) v)))
                     init)))
  #_ILookup
  (#_lookup (coll key) (#_nth coll key #_nil))
  (#_lookup (coll key default) (#_nth coll key default))
  #_IReduce
  (#_internal-reduce (coll f)
                     (fset:reduce f coll))
  (#_internal-reduce (coll f start)
                     (fset:reduce f coll :initial-value start)))

(extend-type map
  #_ISeqable
  (#_seq (map)
         (if (empty? map) #_nil
             (collecting
               (do-map (k v map)
                 (collect (seq k v))))))
  #_ISeq
  (#_first (map)
           (if (empty? map) #_nil
               (do-map (k v map)
                 (return (seq k v)))))
  (#_rest (map) (#_rest (#_seq map)))
  #_INext
  (#_next (map) (#_next (#_seq map)))
  #_IEmptyableCollection
  (#_empty (map) (fset:empty-map))
  #_ICollection
  (#_conj (map x) (apply #'fset:with map (convert 'list (#_seq x))))
  #_IFn
  (#_invoke (x args) (lookup x (only-elt args)))
  #_IAssociative
  (#_contains-key? (map key) (? (fset:contains? map key)))
  (#_assoc (map key value) (with map key value))
  #_IMap
  (#_-dissoc (map key keys)
             (reduce #'less
                     (cons key keys)
                     :initial-value map))
  #_IKVReduce
  (#_kv-reduce (map f init)
               (if (empty? map) map
                   (progn
                     (do-map (k v map)
                       (setf init (ifncall f init k v)))
                     init))))

(extend-type set
  #_ISeqable
  (#_seq (set)
         (if (empty? set) #_nil
             (collecting
               (do-set (x set)
                 (collect x)))))
  #_IEmptyableCollection
  (#_empty (set) (fset:empty-set))
  #_ICollection
  (#_conj (set x) (with set x))
  #_IFn
  (#_invoke (x args) (lookup x (only-elt args))))

;;; In Clojure only keywords (all keywords) and /qualified/ symbols
;;; are interned; unqualified symbols are not interned. Two
;;; unqualified symbols are /never/ identical and, if they have the
;;; same name, are always egal (under =). This matters when symbols
;;; are used as keys in maps.

(extend-type symbol
  #_IHash
  (#_hash (s)
          (if (keywordp s) (murmurhash s)
              (multiple-value-bind (ns name) (ns+name s)
                (if ns (murmurhash s)
                    (murmurhash (cons '%sym name)))))))

(defun-1 #_instance? (class x)
  (? (typep x class)))

(defun-1 #_type (x)
  (type-of x))

(defun-1 #_inc (n)
  (1+ n))

(defun-1 #_dec (n)
  (1- n))

(define-clojure-macro #_locking (x &body body)
  `(synchronized (,x)
     ,@body))

(defun seq-with (seq idx val)
  "Like `fset:with', but IDX must be less than the length of SEQ."
  (if (<= idx (size seq))
      (with seq idx val)
      (error (clojure-program-error "Idx ~a not valid for ~a" idx seq))))

(defun-1 #_alter-meta! (obj f &rest args)
  (synchronized (obj)
    (setf (meta obj)
          (ifn-apply f (#_meta obj) args))))

(define-clojure-macro #_cond (&rest clauses)
  (ematch clauses
    ((list) #_nil)
    ((list test expr)
     `(#_if ,test ,expr #_nil))
    ((list* test expr clauses)
     `(#_if ,test ,expr (#_cond ,@clauses)))))

(define-clojure-macro #_letfn (fnspecs &body body)
  (let* ((fnspecs (convert 'list fnspecs)))
    `(fbindrec ,(loop for (name . body) in fnspecs
                      collect `(,name (#_fn ,name ,@body)))
       (symbol-macrolet ,(loop for (name . nil) in fnspecs
                               collect `(,name #',name))
         ,@body))))

(defun-1 #_gensym (&optional (prefix-string "G__"))
  (gensym prefix-string))

(defun-1 #_get (map key &optional not-found)
  (#_lookup map key not-found))

(defun-1 #_constantly (x)
  (constantly x))

(defun-1 #_alter-var-root (root f &rest args)
  (synchronized (root)
    (setf (symbol-value root)
          (ifn-apply f (symbol-value root) args))))

(defvar *assert* t)
(expose-to-clojure #_*assert* *assert*)

(define-clojure-macro #_assert (test &optional message)
  `(when *assert*
     (assert ,test () ,@(unsplice message))))

(defun-1 #_special-symbol? (s)
  (? (memq s special-forms)))

(define-clojure-macro #_declare (&rest args)
  (declare (ignore args)))

(defun-1 #_= (&rest args)
  (assure clojure-boolean
    (match args
      ((list) #_true)
      ((list _) #_true)
      ((list x y) (if (eql x y) #_true (#_equiv x y)))
      (otherwise
       (? (loop for x in args
                for y in (rest args)
                always (or (eql x y)
                           (truthy? (#_equiv x y)))))))))

(defun not= (&rest args)
  (truthy? (apply #'#_not= args)))

(defun-1 #_not= (&rest args)
  (#_not (apply #'#_= args)))

(defun-1 #_== (&rest args)
  (apply #'= args))

(define-clojure-macro #_comment (&body body)
  (declare (ignore body))
  (values))

(defun-1 #_list (&rest items)
  items)

(defun-1 #_list* (&rest items)
  ;; TODO non-lists
  (cond ((null items))
        ((single items) (first items))
        (t (let ((last1 (lastcar items)))
             (if (listp last1)
                 (apply #'list* items)
                 (fset:reverse
                  (fset:concat (fset:reverse items)
                               (reverse (butlast items)))))))))

(defun-1 #_bit-and (x y &rest more)
  (apply #'logand x y more))

(defun-1 #_bit-or (x y &rest more)
  (apply #'logior x y more))

(defun-1 #_bit-xor (x y &rest more)
  (apply #'logxor x y more))

(defun-1 #_bit-not (x)
  (lognot x))

(defun-1 #_re-find (re str)
  (ematch (multiple-value-list (ppcre:scan re str))
    ((list nil) #_nil)
    ((list start end (and _ (satisfies emptyp)) _)
     (subseq str start end))
    ((list start end starts ends)
     (convert 'seq
              (cons (subseq str start end)
                    (cl:map 'list
                            (lambda (start end)
                              (subseq str start end))
                            starts ends))))))

(define-compiler-macro #_re-find (&whole call re str)
  (typecase re
    (regex
     (let ((re (regex-string re)))
       `(#_re-find (load-time-value (ppcre:create-scanner ,re))
                   ,str)))
    (otherwise call)))

(defun-1 #_re-matches (re str)
  (ematch (multiple-value-list (ppcre:scan-to-strings re str))
    ((list nil) #_nil)
    ((list string (and _ (satisfies emptyp)))
     string)
    ((list string groups)
     (convert 'seq (cons string (coerce 'list groups))))))

(define-compiler-macro #_re-matches (&whole call re str)
  (typecase re
    (regex
     (let* ((re (regex-string re))
            (re (string+ "^" re "$")))
       `(#_re-find (load-time-value (ppcre:create-scanner ,re))
                   ,str)))
    (otherwise call)))

(defun-1 #_re-pattern (s)
  (ppcre:create-scanner (assure string s)))

(define-clojure-macro #_defmulti (name &body body)
  (mvlet* ((body docs (body+docs+attrs body)))
    (declare (ignore docs))             ;TODO
    (ematch body
      ((list* dispatch-fn args)
       `(defalias-1 ,name
            (make 'multimethod
                  :name ',name
                  :fn (ifn-function ,dispatch-fn)
                  ,@args))))))

(define-clojure-macro #_defmethod (name value params &body body)
  ;; Debugging is easier if we can see which dispatch value in the
  ;; backtrace.
  (let ((fn-name (gensym (string+ name " " value))))
    `(progn
       (add-clojure-method (symbol-function ',name) ,value
                           (#_fn ,fn-name ,params ,@body))
       ',name)))

(defun-1 #_class (x)
  (class-of x))

(defun-1 #_class? (x)
  (typep x 'class))

(defun-1 #_identity (x) x)

(defun-1 #_rand (&optional (n 1))
  (random (coerce n 'double-float)))

(defun-1 #_rand-int (n)
  (values (round (random n))))

(defun-1 #_< (&rest xs)  (? (apply #'< xs)))
(defun-1 #_> (&rest xs)  (? (apply #'> xs)))
(defun-1 #_>= (&rest xs) (? (apply #'>= xs)))
(defun-1 #_<= (&rest xs) (? (apply #'<= xs)))
(defun-1 #_+ (&rest xs) (apply #'+ xs))
(defun-1 #_- (&rest xs) (apply #'- xs))
(defun-1 #_* (&rest xs) (apply #'* xs))
(defun-1 #_/ (&rest xs) (apply #'/ xs))

(defun-1 #_mod (num div) (mod num div))
(defun-1 #_rem (num div) (rem num div))

(defun-1 #_all-ns ()
  (list-all-packages))

(defun-1 #_empty? (xs)
  (#_not (#_seq xs)))

(defun-1 #_reverse (xs)
  (typecase xs
    (sequence (reverse xs))
    (lazy-seq (#_reverse (lazy-seq->list xs)))
    (t (#_rseq xs))))

(defun-1 #_ns-name (ns)
  (intern (package-name ns)))

(defun-1 #_the-ns (name)
  (find-package name))

(defun-1 #_ns-resolve (ns sym)
  (declare (ignore ns))
  (macroexpand (assure symbol sym)))

(defun-1 #_resolve (sym)
  (#_ns-resolve #_*ns* sym))

(define-clojure-macro #_if-let (bindings &body (then &optional else))
  (ematch bindings
    ((seq binds test)
     (with-unique-names (temp)
       `(let ((,temp ,test))
          (#_if ,temp
                (#_let ,(seq binds temp)
                       ,then)
                ,else))))))

(define-clojure-macro #_when-let (bindings &body body)
  `(#_if-let ,bindings (#_do ,@body)))

(defun-1 #_symbol (ns &optional name)
  (if (not (nil? name))
      (intern name (#_the-ns ns))
      (intern name)))

(defun-1 #_symbol? (x)
  (? (and (symbolp x) (not (keywordp x)))))

(defun-1 #_fnil (f x &optional
                   (y #_nil y-supplied?)
                   (z #_nil z-supplied?))
  (cond (z-supplied?
         (lambda (arg1 arg2 arg3 &rest args)
           (ifn-apply f
                      (if (nil? arg1) x arg1)
                      (if (nil? arg2) y arg2)
                      (if (nil? arg3) z arg3)
                      args)))
        (y-supplied?
         (lambda (arg1 arg2 &rest args)
           (ifn-apply f
                      (if (nil? arg1) x arg1)
                      (if (nil? arg2) y arg2)
                      args)))
        (t
         (lambda (arg1 &rest args)
           (ifn-apply f
                      (if (nil? arg1) x arg1)
                      args)))))

(defun-1 #_new (class &rest args)
  (apply #'make-instance class args))

(define-clojure-macro #_case (e &body clauses)
  (mvlet* ((default? (evenp (length clauses)))
           (clauses (batches clauses 2))
           (clauses default
            (if default?
                (values clauses nil)
                (values (butlast clauses) (lastcar clauses)))))
    `(case-using #'#_= ,e
       ,@clauses
       ,@(unsplice (and default? `(otherwise ,default))))))

(defun-1 #_comp (&rest fns)
  (if (null fns) #'identity
      (apply #'compose (mapcar #'ifn-function fns))))

(defun-1 #_vals (map)
  (collecting
    (do-map (k v map)
      (declare (ignore k))
      (collect v))))

(defun-1 #_keys (map)
  (collecting
    (do-map (k v map)
      (declare (ignore v))
      (collect k))))

(defun-1 #_nthnext (coll n)
  (assert (not (minusp n)))
  (nlet nthnext ((coll coll)
                 (n n))
    (if (zerop n)
        (#_seq coll)
        (nthnext (#_next coll) (1- n)))))

(defun-1 #_nthrest (coll n)
  (assert (not (minusp n)))
  (nlet rec ((coll coll)
             (n n))
    (if (zerop n)
        (#_seq coll)
        (rec (#_next coll) (1- n)))))

(define-clojure-macro #_time (form)
  `(time ,form))

(defun-1 #_memoize (f)
  (let ((table (make-egal-hash-table))
        (fun (ifn-function f)))
    (lambda (&rest args)
      (values
       (synchronized (table)
         (ensure2 (gethash args table)
           (apply fun args)))))))

(defunit unrealized)

(defstruct (memo-cell
            (:constructor memo-cell (thunk)))
  (value unrealized)
  (thunk (error "No thunk!") :type function)
  (lock (bt:make-lock) :read-only t))

(defun force (x)
  (if (not (typep x 'memo-cell)) x
      (with-accessors ((value memo-cell-value)
                       (thunk memo-cell-thunk)
                       (lock  memo-cell-lock))
          x
        (if (eq value unrealized)
            (synchronized (lock)
              (if (eq value unrealized)
                  (setf value (funcall (shiftf thunk #'identity)))
                  value))
            value))))

(defun forced? (memo-cell)
  (not (eq unrealized (memo-cell-value memo-cell))))

(defstruct (delay
             (:include memo-cell)
             (:constructor make-delay (thunk))))

(define-clojure-macro #_delay (&body body)
  `(make-delay (lambda () ,@body)))

(defun-1 #_delay? (x)
  (? (typep x 'delay)))

(defun-1 #_force (x)
  (if (typep x 'delay) (force x) x))

(extend-type delay
  #_IDeref
  (#_deref (x) (force x))
  #_IPending
  (#_realized? (x) (? (forced? x))))

(declaim (inline make-lazy-seq))        ;It might be useful to stack-allocate?
(defstruct (lazy-seq
             (:include memo-cell)
             (:constructor make-lazy-seq (thunk))))

(defmacro lazy-seq (&body body)
  `(make-lazy-seq (lambda () ,@body)))

(define-clojure-macro #_lazy-seq (&body body)
  `(lazy-seq ,@body))

(extend-type lazy-seq
  #_IDeref
  (#_deref (x) (force x))
  #_IPending
  (#_realized? (x) (? (forced? x)))
  #_ISequential
  #_ISeq
  (#_first (x) (#_first (force x)))
  (#_rest (x) (#_rest (force x)))
  #_INext
  (#_next (x) (#_next (force x)))
  #_ISeqable
  (#_seq (x) (#_seq (force x)))
  #_IEmptyableCollection
  (#_empty (seq) '())
  #_IEquiv
  (#_equiv (self other)
           (#_= (force self) (force other)))
  #_IReduce
  (#_internal-reduce (coll f)
                     (let ((f (ifn-function f))
                           (init nil)
                           (init? nil))
                       (mapr (lambda (seq)
                               (if (not init?)
                                   (setf init (#_first seq)
                                         init? t)
                                   (setf init (funcall f init (#_first seq)))))
                             coll)))
  (#_internal-reduce (coll f start)
                     (let ((f (ifn-function f))
                           (init start))
                       (mapr (lambda (seq)
                               (setf init (funcall f init (#_first seq))))
                             coll))))

(defun-1 #_cons (x y)
  (cons x y))

(defun doall-n (n seq)
  (nlet doall-n ((n n)
                 (seq seq))
    (and (plusp n)
         (seq? seq)
         (doall-n (1- n)
                  (#_next seq)))))

(defun doall (seq)
  (nlet doall ((seq seq))
    (and (seq? seq)
         (doall (#_next seq)))))

(defun-1 #_doall (&rest args)
  (ematch args
    ((list seq) (doall seq) seq)
    ((list n seq) (doall-n n seq) seq)))

(defun-1 #_dorun (&rest args)
  ;; TODO avoid consing
  (apply #_doall args)
  (values))

(defun lazy-seq->list (lazy-seq)
  (nlet rec ((seq lazy-seq)
             (acc '()))
    (if (not (seq? seq))
        (nreverse acc)
        (rec (#_rest seq)
             (cons (#_first seq) acc)))))

(defmethod convert ((type (eql 'list)) (seq lazy-seq) &key)
  (lazy-seq->list seq))

(defun-1 #_concat (&rest seqs)
  (labels ((rec (seqs)
             (if (null seqs) '()
                 (let ((seq1 (first seqs)))
                   (if (seq? seq1)
                       (lazy-seq
                         (cons (#_first seq1)
                               (apply #'#_concat
                                      (#_rest seq1)
                                      (rest seqs))))
                       (rec (rest seqs)))))))
    (rec seqs)))

(defun-1 #_cycle (seq)
  (labels ((rec (tail)
             (if (seq? tail)
                 (#_cons (#_first tail)
                         (lazy-seq (rec (#_rest tail))))
                 (rec seq))))
    (rec seq)))

(defun-1 #_take (n seq)
  (if (and (plusp n)
           (seq? seq))
      (lazy-seq (cons (#_first seq)
                      (#_take (1- n) (#_rest seq))))
      '()))

(defun-1 #_drop-while (pred seq)
  (fbind ((pred (ifn-function pred)))
    (if (not (seq? seq)) '()
        (lazy-seq
          (multiple-value-bind (first rest) (first+rest seq)
            (if (truthy? (pred first))
                (#_drop-while pred rest)
                seq))))))

(defun zip (seqs)
  (if (notevery #'seq? seqs) '()
      (lazy-seq
        (cons (mapcar #_first seqs)
              (zip (mapcar #_rest seqs))))))

(defun map/1 (fn col)
  (labels ((map/1 (fn col)
             (if (not (seq? col)) '()
                 (lazy-seq
                   (cons (funcall fn (#_first col))
                         (map/1 fn (#_rest col)))))))
    (map/1 (ifn-function fn) col)))

(defun map/n (fn &rest cols)
  (let ((fn (ifn-function fn)))
    (map/1 (lambda (args) (apply fn args))
           (zip cols))))

(defun-1 #_map (fn &rest colls)
  (ematch colls
    ((list) '())
    ((list coll) (map/1 fn coll))
    ((list* _ _) (apply #'map/n fn colls))))

(defun-1 #_filter (pred coll)
  (fbind ((pred (ifn-function pred)))
    (if (not (seq? coll)) '()
        (lazy-seq
          (nlet filter* ((coll coll))
            (if (not (seq? coll)) '()
                (multiple-value-bind (first rest) (first+rest coll)
                  (if (truthy? (pred first))
                      (cons first (#_filter pred rest))
                      (filter* rest)))))))))

(defun repeatedly (fn &optional n)
  (fbind ((fn (ifn-function fn)))
    (if (null n)
        (lazy-seq (cons (fn) (lazy-seq (repeatedly fn))))
        (labels ((rec (n)
                   (if (zerop n) '()
                       (lazy-seq (cons (fn) (repeatedly fn (1- n)))))))
          (rec n)))))

(defun-1 #_repeatedly (&rest args)
  (ematch args
    ((list n fn)
     (repeatedly fn n))
    ((list fn)
     (repeatedly fn))))

(defun-1 #_repeat (n-or-x &optional x)
  (multiple-value-bind (n x)
      (if (nil? x)
          (values nil n-or-x)
          (values n-or-x x))
    (if (null n)
        (#_repeatedly (#_constantly x))
        (#_repeatedly n (#_constantly x)))))

(defunit infinity)

(defun-1 #_range (&rest args)
  (ematch args
    ((list start (and _ (eql start)) _)
     '())
    ((list start _ 0)
     (#_repeat start))
    ((list start (and end (eq infinity)) step)
     (let ((next (+ start step)))
       (lazy-seq (cons start (#_range next end step)))))
    ((list start end step)
     (let ((next (+ start step)))
       (if (< next end)
           (lazy-seq (cons start (#_range next end step)))
           (list start))))
    ((list start end)
     (#_range start end 1))
    ((list end)
     (#_range 0 end 1))
    ((list)
     (#_range 0 infinity 1))))

(defun-1 #_pr (&rest xs)
  (format t "~{~s~^ ~}" xs)
  #_nil)

(defun-1 #_prn (&rest xs)
  (apply #_pr xs)
  (terpri)
  #_nil)

(defun-1 #_print (&rest more)
  (format t "~{~a~^ ~}" more)
  #_nil)

(defun-1 #_println (&rest more)
  (apply #_print more)
  (terpri)
  #_nil)

(defun-1 #_print-str (&rest more)
  (with-output-to-string (*standard-output*)
    (apply #_print more)))

(defun-1 #_pr-str (&rest more)
  (with-output-to-string (*standard-output*)
    (apply #_pr more)))

(define-clojure-macro #_with-out-str (&body body)
  `(with-output-to-string (*standard-output*)
     ,@body))

(defun-1 #_interpose (sep coll)
  (if (not (seq? coll)) '()
      (if (not (seq? (#_rest coll)))
          coll
          (#_concat
           (list (#_first coll)
                 sep)
           (#_interpose sep (#_next coll))))))

(defun-1 #_group-by (fn seq)
  (let* ((fn (ifn-function fn))
         (seq (convert 'list seq))
         (groups (assort seq :key fn :test (compose #'truthy? #_=)))
         (map (empty-map)))
    (dolist (group groups map)
      (let ((key (funcall fn (first group))))
        (withf map key (convert 'seq group))))))

(defun-1 #_merge-with (fn &rest maps)
  (let ((fn (ifn-function fn)))
    (reduce (lambda (map1 map2)
              ;; NB fset:map-union does not have the right semantics.
              (do-map (key val2 map2 map1)
                (multiple-value-bind (val1 val1?)
                    (fset:lookup map1 key)
                  (if val1?
                      (withf map1 key (funcall fn val1 val2))
                      (withf map1 key val2)))))
            maps
            :initial-value (empty-map))))

(defun-1 #_merge (&rest maps)
  (apply #_merge-with #'second maps))

(defconstructor #_reduced
  (value t))

(extend-type #_reduced
  #_IDeref
  (#_deref (x)
           (match x
             ((#_reduced v) v))))

(defun-1 #_reduced? (x)
  (? (typep x '#_reduced)))

(defun-1 #_unreduced (x)
  (if (typep x '#_reduced)
      (#_deref x)
      x))

(defun call/reduced (body-fn reduce-fn)
  (let ((reduce-fn (ifn-function reduce-fn)))
    (funcall body-fn
             (lambda (&rest args)
               (let ((result
                       (ematch args
                         ((list) (funcall reduce-fn))
                         ((list init next)
                          (funcall reduce-fn init next)))))
                 (#_if (#_reduced? result)
                       (return-from call/reduced
                         (#_unreduced result))
                       result))))))

(defmacro with-reduced ((f reduce-fn) &body body)
  (with-thunk (body f)
    `(call/reduced ,body ,reduce-fn)))

(defun-1 #_reduce (&rest args)
  (ematch args
    ((list f coll)
     (with-reduced (f f)
       (#_internal-reduce (#_seq coll) f)))
    ((list f start coll)
     (with-reduced (f f)
       (#_internal-reduce (#_seq coll) f start)))))

(defun reduce-rests (coll f start start?)
  (let ((f (ifn-function f))
        (init start)
        (init? start?))
    (mapr (lambda (elt)
            (if (not init?)
                (setf init elt
                      init? t)
                (setf init (funcall f init elt))))
          coll)
    init))

(defun get-once-value (name)
  (let* ((unbound "unbound")
         (val (get name 'once-value unbound)))
    (if (eq unbound val)
        (values nil nil)
        (values val t))))

(defun (setf get-once-value) (value name)
  (setf (get name 'once-value) value))

(define-clojure-macro #_defonce (name value)
  `(#_def ,name (ensure2 (get-once-value ',name) ,value)))

(defun-1 #_name (x)
  (etypecase x
    (string x)
    (symbol (unqualify-symbol x))))

(define-clojure-macro #_try (&body forms)
  (flet ((catcher->handler (catcher)
           (multiple-value-bind (classname name exprs)
               (ematch catcher
                 ((list* '#_catch :|default| name exprs)
                  (values 'error name exprs))
                 ((list* '#_catch classname name exprs)
                  (values classname name exprs)))
             `(,classname (,name)
                          ,@(unsplice (and (string= name '_) `(declare (ignorable ,name))))
                          ,@exprs))))
    (let* ((catch-forms (keep '#_catch forms :key #'car-safe))
           (finally-forms (keep '#_finally forms :key #'car-safe))
           (exprs (remove-if (lambda (form)
                               (or (member form catch-forms)
                                   (member form finally-forms)))
                             forms))
           (expr `(progn ,@exprs))
           (expr
             (if finally-forms
                 `(unwind-protect
                       ,expr
                    ,@(mapcar #'rest finally-forms))
                 expr))
           (expr
             (if catch-forms
                 `(handler-case
                      ,expr
                    ,@(mapcar #'catcher->handler catch-forms))
                 expr)))
      expr)))

(declaim (inline make-atom))
(atomics:defstruct atom
  (value (error "No value!") :type t)
  (validator (constantly t) :type function :read-only t))

(extend-type atom
  #_IDeref
  (#_deref (x) (atom-value x)))

(defun-1 #_atom (value &key meta validator)
  (lret* ((validator (if (nil? validator) (constantly t) validator))
          (atom (make-atom :validator validator
                           :value value)))
    (unless (nil? meta)
      (setf (meta atom) meta))))

(defun-1 #_reset! (atom value)
  (prog1 value
    (setf (atom-value atom) value)))

(defun-1 #_swap! (atom f &rest args)
  (let ((f (ifn-function f)))
    (atomics:atomic-update (atom-value atom)
                           (lambda (old-val)
                             (apply f old-val args)))))

(defvar *syncing* nil)

(defun check-transaction ()
  (unless *syncing*
    (error (#_IllegalStateException. "No transaction running!"))))

(define-clojure-macro #_io! (&body body)
  `(progn
     (when *syncing*
       (error (#_IllegalStateException. "IO in transaction!")))
     ,@body))

(define-clojure-macro #_dosync (&body body)
  `(let ((*syncing* t))
     (stmx:atomic ,@body)))

;;; SBCL doesn't like it if this is a struct.
(stmx:transactional
    (defclass ref ()
      ((value
        :initform (error "No value!")
        :initarg :value
        :type t
        :accessor ref-value)
       (validator
        :initform (constantly t)
        :transactional nil
        :type function
        :reader ref-validator))))

(defun make-ref (&rest args)
  (apply #'make 'ref args))

(extend-type ref
  #_IDeref
  (#_deref (x) (ref-value x)))

(defun-1 #_ref (x &key meta validator min-history max-history)
  (declare (ignore min-history max-history))
  (lret* ((validator (if (nil? validator) (constantly t) validator))
          (ref (make-ref :value x :validator validator)))
    (unless (nil? meta)
      (setf (meta ref) meta))))

(defun-1 #_alter (ref f &rest args)
  (let ((f (ifn-function f)))
    (#_ref-set ref (apply f (ref-value ref) args))))

(defun-1 #_commute (ref f &rest args)
  ;; TODO
  (apply #_alter ref f args))

(defun-1 #_ref-set (ref value)
  (check-transaction)
  (unless (funcall (ref-validator ref) value)
    (error (clojure-error "Cannot set ref ~a to value ~a"
                          ref value)))
  (setf (ref-value ref) value))

(defvar *agent-pool* nil)
(defun ensure-agent-pool ()
  (or *agent-pool*
      (synchronized ('*agent-pool*)
        (or *agent-pool*
            (setf *agent-pool*
                  (lparallel:make-kernel (+ 2 (overlord:nproc))
                                         :name "Agent kernel"))))))

(defun call/agent-pool (fn)
  (let ((lparallel:*kernel* (ensure-agent-pool)))
    (funcall fn)))

(defmacro with-agent-pool ((&key) &body body)
  (with-thunk (body)
    `(call/agent-pool ,body)))

(deftype agent-error-mode ()
  '(member :fail :continue))

(defstruct agent
  (state (error "No state!"))
  (validator (constantly t))
  (error #_nil)
  (error-mode :fail)
  (error-handler #_nil))

(defun signal-agent-error (agent)
  (when-let (e (agent-error agent))
    (error e)))

(defun-1 #_agent (state &key meta validator error-handler error-mode)
  (lret* ((error-mode
           (if (nil? error-mode)
               (if (nil? error-handler) :fail :continue)
               error-mode))
          (error-handler
           (unless (nil? error-handler)
             (ifn-function error-handler)))
          (validator (if (nil? validator) (constantly t) (ifn-function validator)))
          (agent
           (make-agent :state state
                       :validator validator
                       :error-mode error-mode
                       :error-handler error-handler)))
    (when (eql error-mode :|continue|)
      (assert (functionp error-handler)))
    (unless (nil? meta)
      (setf (meta agent) meta))))

(defun-1 #_agent-error (agent)
  (agent-error agent))

(defun-1 #_error-handler (agent)
  (agent-error-handler agent))

(defun-1 #_error-mode (agent)
  (agent-error-mode agent))

(extend-type agent
  #_IDeref
  (#_deref (agent)
           (signal-agent-error agent)
           (agent-state agent)))

(defun update-agent (agent f &rest args)
  (handler-case
      (setf (agent-state agent)
            (apply f (agent-state agent) args))
    (error (e)
      (ecase-of agent-error-mode (agent-error-mode agent)
        (:fail (setf (agent-error agent) e))
        (:continue (funcall (agent-error-handler agent) e))))))

(defun #_send (agent f &rest args)
  ;; TODO
  (signal-agent-error agent)
  (with-agent-pool ()
    (lparallel:future
      (setf (agent-state agent)
            (apply f (agent-state agent) args))))
  agent)

(defun #_send-off (agent f &rest args)
  (signal-agent-error agent)
  (bt:make-thread
   (lambda ()
     (setf (agent-state agent)
           (apply f (agent-state agent) args)))
   :name "Send-off thread")
  agent)

(defun-1 #_restart-agent (agent state)
  (synchronized (agent)
    (setf (agent-state agent) state
          (agent-error agent) #_nil)))

(defstruct future
  (lp-future (error "No future!")))

(define-clojure-macro #_future (&body body)
  `(with-agent-pool ()
     (make-future :lp-future (lparallel:future ,@body))))

(extend-type future
  #_IDeref
  (#_deref (f) (lparallel:force f))
  #_IPending
  (#_realized? (f) (lparallel:fulfilledp f)))

(defstruct promise
  (lp-promise (lparallel:promise)))

(defun-1 #_promise ()
  (make-promise))

(defun-1 #_deliver (promise value)
  (lparallel:fulfill promise value))

(extend-type promise
  #_IDeref
  (#_deref (p) (lparallel:force p))
  #_IPending
  (#_realized? (p) (lparallel:fulfilledp p)))

(defun mapstrict (fn seq)
  "Map FN over SEQ, a Clojure seq."
  (nlet rec ((seq seq)
             (acc nil))
    (if (not (seq? seq))
        (nreverse acc)
        (rec (#_rest seq)
             (cons (funcall fn (#_first seq))
                   acc)))))

(defloop mapr (fn seq)
  (when (seq? seq)
    (funcall fn (#_first seq))
    (mapr fn (#_rest seq))))

(defun maprest (fn seq)
  (collecting (mapr (compose #'collect fn) seq)))

(define-do-macro doseq ((var seq &optional ret) &body body)
  (let ((decls
          (and (string= '_ var)
               `((declare (ignore ,var))))))
    `(mapr (lambda (,var) ,@decls ,@body) ,seq)))

(define-clojure-macro #_doseq (binds &body body)
  (ematch (convert 'list binds)
    ((list pat seq)
     (with-unique-names (temp)
       `(doseq (,temp ,seq #_nil)
          (#_let ,(seq pat temp)
                 ,@body))))
    ((list* pat seq binds)
     `(#_doseq ,(seq pat seq)
               (#_doseq ,(convert 'seq binds)
                        ,@body)))))

(defun call/for (fn seq)
  (if (seq? seq)
      (lazy-seq
        (cons (funcall fn (#_first seq))
              (call/for fn (#_rest seq))))
      empty-list))

(define-clojure-macro #_for (binds &body body)
  ;; TODO Fork cl-lc and actually implement list comprehensions.
  (ematch binds
    ((seq pat form)
     (with-unique-names (temp)
       `(call/for (lambda (,temp)
                    (#_let ,(seq pat temp)
                           ,@body))
                  ,form)))))

(defun-1 #_assoc-in (m ks v)
  (#_update-in m ks (constantly v)))

(defun-1 #_update-in (m ks f &rest args)
  (let ((ks (convert 'list ks))
        (f (ifn-function f)))
    (labels ((rec (m ks)
               (let ((m (if (nil? m) (empty-map) m)))
                 (match ks
                   ((list) m)
                   ((list key)
                    (let* ((old (#_lookup m key))
                           (new (apply f old args)))
                      (#_assoc m key new)))
                   ((list* key keys)
                    (#_assoc m key (rec (#_lookup m key) keys)))))))
      (rec m ks))))

(defun-1 #_get-in (m ks &optional not-found)
  (nlet get-in ((m m)
                (ks ks))
    (if (not (seq? ks)) m
        (let ((k (#_first ks))
              (ks (#_rest ks)))
          (get-in (#_lookup m k not-found) ks)))))

(defun-1 #_subvec (v start &optional end)
  (if (nil? end)
      (fset:subseq v start)
      (fset:subseq v start end)))

(defun-1 #_read-string (&rest args)
  (ematch args
    ((list string)
     (#_read-string (empty-map) string))
    ((list map string)
     (let* ((*readtable* (find-readtable 'cloture))
            (eof (#_lookup map :|eof| :|eofthrow|))
            (eof-error? (eql :|eof| :|eofthrow|)))
       (read-from-string string eof-error? eof)))))

(defun-1 #_set? (x)
  (? (typep x 'set)))

(defun-1 #_map? (x)
  (? (typep x 'map)))

(defun-1 #_qualified-symbol? (x)
  (? (and (symbolp x)
          (not (keywordp x))
          (ns+name x))))

(defun-1 #_qualified-keyword? (x)
  (? (and (keywordp x) (ns+name x))))

(defun-1 #_ident? (x)
  (? (symbolp x)))

(defun-1 #_qualified-ident? (x)
  (? (and (symbolp x) (ns+name x))))

(defun-1 #_simple-ident? (x)
  (? (and (symbolp x) (ns+name x))))

(define-clojure-macro #_dotimes (bindings &body body)
  (ematch bindings
    ((seq i n)
     `(dotimes (,i ,n)
        (with-syms-fbound (,i)
          ,@body)))))

(defunit not-there)

(defun-1 #_contains? (coll key)
  (? (not (eq not-there (#_get coll key not-there)))))

(defun-1 #_aget (array idx &rest idxs)
  (apply #'aref array idx idxs))

(defun-1 #_aclone (array)
  (copy-array array))

(defun-1 #_vec (x)
  (convert 'seq x))

(defun-1 #_partial (fn &rest args)
  (apply #'partial fn args))

(define-clojure-macro while (test &body body)
  `(loop while (truthy? ,test) do ,@body))

(-> #_int (t) integer)
(defun-1 #_int (x)
  (etypecase x
    (character (char-code x))
    (number (coerce x 'integer))))

(-> #_float (t) double-float)
(defun-1 #_float (x)
  (coerce x 'double-float))

(defun-1 #_floats (xs)
  (replace (make-array (#_count xs) :element-type 'double-float)
           (mapstrict (op (coerce _ 'double-float))
                      xs)))

(defun-1 #_disj (set key)
  (fset:less (assure set set) key))

(defclass transient ()
  ((coll :initarg :coll :type t
         :accessor transient-coll)
   (persistent? :initform nil :type boolean
                :accessor transient-persistent?)))

(defclass transient-vector (transient)
  ((coll :type seq))
  (:default-initargs :coll (empty-seq)))

(defclass transient-map (transient)
  ((coll :type map))
  (:default-initargs :coll (empty-map)))

(defclass transient-set (transient)
  ((coll :type set))
  (:default-initargs :coll (empty-set)))

(extend-protocol #_IEditableCollection
  seq
  (#_as-transient (seq)
                  (make 'transient-vector :coll seq))
  map
  (#_as-transient (map)
                  (make 'transient-map :coll map))
  set
  (#_as-transient (set)
                  (make 'transient-set :coll set)))

(defun-1 #_transient (coll)
  (#_as-transient coll))

(defun check-not-persistent (transient)
  (when (transient-persistent? transient)
    (error 'already-persistent
           :transient transient)))

(defun-1 #_persistent! (transient)
  (check-not-persistent transient)
  (with-slots (coll persistent?) transient
    (prog1 (shiftf coll (#_empty coll))
      (setf persistent? t))))

(defun-1 #_conj! (&optional (coll nil coll-supplied?)
                            (x nil x-supplied?))
  (cond ((not coll-supplied?)
         (make 'transient-vector :coll (seq)))
        ((not x-supplied?)
         coll)
        (t
         (prog1 coll
           (setf (transient-coll coll)
                 (conj (transient-coll coll) x))))))
