(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Note that there are many special cases here that could be compiled more efficiently or inlined, or macro-expanded more legibly. For now simplicity & uniformity is the goal. In the future, maybe, when there is more code to test against, optimization might be worthwile. Not yet.

;;; TODO make Lisp lambda lists and Clojure arg vectors congruent when possible.

;;; Not inlining these would be criminal.
(declaim (inline . #_(bit-and bit-or bit-xor bit-not bit-flip bit-set bit-shift-right bit-shift-left bit-and-not bit-clear bit-test unsigned-bit-shift-right)))

(declaim (inline . #_(cons < > >= <= + - / * mod rem)))

(defconst special-forms
  '#_(quote
      if do def let binding var
      loop recur throw try))

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

(defun-1 #_identical (x y)
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
             (forms (string-gensym 'forms))
             (env args (split-args-on args '&environment))
             (whole args (split-args-on args '&whole)))
      `(defmacro ,name (,@(and whole `(&whole ,whole))
                        &rest ,forms
                              ,@(and env `(&environment ,env)))
         ,@(unsplice docs)
         (declojurize
          (block ,name                  ;catch return-from
            (let ((,forms (clojurize ,forms))
                  ,@(unsplice (and whole `(,whole (clojurize ,whole)))))
              (destructuring-bind ,args ,forms
                ,@decls
                ,@body))))))))

(defun special-form? (form)
  (and (consp form)
       (symbolp (car form))
       (member (car form) special-forms)))

(defun-1 #_macroexpand-1 (form)
  (if (special-form? form) form
      (clojurize (macroexpand-1 form))))

(defun-1 #_macroexpand (form)
  (loop for f = form then exp
        for exp = (#_macroexpand-1 f)
        until (eq f exp)
        finally (return f)))

(define-clojure-macro #_if (test then &optional (else #_nil))
  `(if (truthy? ,test) ,then ,else))

(define-clojure-macro #_if-not (test then &optional (else #_nil))
  `(if (falsy? ,test) ,then ,else))

(define-clojure-macro #_do (&rest exprs)
  `(progn ,@exprs))

;;; Here's how dynamic variables work. In Clojure `let' always
;;; produces lexical variables, only `binding' can rebind variables.
;;; So "dynamic" variables are defined as a symbol macro with a
;;; backing Lisp special (the "var"). Using `let' just rebinds the
;;; symbol macro. But `binding' expands the symbol macro.

(defconst clojure-var-prefix '*clojure-var-)

(define-clojure-macro defprivate (name &body body)
  "Define a (private) var."
  (mvlet* ((docstring expr
            (ematch body
              ((list (and docstring (type string))
                     expr)
               (values docstring expr))
              ((list expr)
               (values nil expr))))
           (dynamic? (meta-ref name :|dynamic|))
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
       (defun ,name (&rest args)
         (ifn-apply ,name args))
       ',name)))

(define-clojure-macro #_def (name &body body)
  (mvlet* ((private? (meta-ref name :|private|)))
    `(progn
       (defprivate ,name ,@body)
       ,@(unsplice
          (unless private?
            `(export ',name ,(package-name (symbol-package name)))))
       ',name)))

(defun-1 #_ns-interns (ns)
  (let ((map (empty-map)))
    (do-symbols (s ns)
      (when-let (var (find-var s))
        (withf map s var)))
    map))

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
  (let* ((bindings (convert 'list bindings)))
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
  (find-var symbol))

(defun-1 #_var-get (var)
  (symbol-value var))

(define-clojure-macro #_defn (name &body body)
  (mvlet ((body docs (body+docs+attrs body)))
    `(#_def ,name ,@(unsplice docs)
            (#_fn ,name ,@body))))

(define-clojure-macro #_defn- (name &body body)
  (mvlet ((body docs (body+docs+attrs body)))
    `(defprivate ,name ,@(unsplice docs)
       (#_fn ,name ,@body))))

(define-clojure-macro #_fn (&body body*)
  (local
    (defun fn-clause->binding (c args-sym)
      `(nlet %recur ((,args-sym ,args-sym))
         (#_let ,(seq (fn-clause-params c) args-sym)
                ,@(require-body-for-splice (fn-clause->body c)))))

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
    (def max-arity (reduce #'max clauses :key #'fn-clause-min-arity))
    (def variadic-clauses (filter #'fn-clause-rest clauses))

    (when (length> variadic-clauses 1)
      (error (clojure-syntax-error "Only one variadic overloard in an fn.")))

    (def variadic (first variadic-clauses))
    (setf clauses (remove variadic clauses))

    (def args-len (string-gensym 'len))

    (def dispatch
      ;; TODO Only check length up to max arity (+1)?
      `(let ((,args-len (length ,args-sym)))
         (if (> ,args-len ,max-arity)
             ,(if variadic
                  (fn-clause->binding variadic args-sym)
                  `(too-many-arguments ,max-arity ,args-sym))
             (ecase ,args-len
               ,@(loop for clause in clauses
                       collect `(,(fn-clause-min-arity clause)
                                 ,(fn-clause->binding clause args-sym)))))))

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
  (if (not (string*= "/" symbol)) symbol
      (intern (ematch (symbol-name symbol)
                ((ppcre "(.+?)/(.+)" _ symbol-name)
                 symbol-name))
              (symbol-package symbol))))

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
                    (:all (#_refer lib))
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
       (defpackage ,(string name)
         (:use)
         ,@(unsplice docstr))
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
                          `(apply #_refer-clojure (#_quote ,args)))
                         ((list* :|refer-cl| args)
                          `(apply #_refer-cl (#_quote ,args)))))
       (find-package ,name))))

(define-clojure-macro #_in-ns (name)
  `(in-package ,(string name)))

(define-clojure-macro #_defmacro (name &body body)
  (mvlet ((body docstr (body+docs+attrs body))
          (forms (string-gensym 'forms)))
    `(define-clojure-macro ,name (&whole #_&form
                                         &body ,forms
                                         &environment #_&env)
       ,@(unsplice docstr)
       (declare (ignorable #_&form #_&env))
       (declojurize
        (apply (#_fn ,@body)
               (clojurize ,forms))))))

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

(defmacro defprotocol (name &body specs)
  (mvlet ((sigs doc (body+docs+attrs specs)))
    `(progn
       (eval-always
         (setf (symbol-protocol ',name)
               (protocol ',name ',sigs))
         ,@(unsplice
            (when doc
              `(setf (documentation ',name 'protocol)
                     ,doc))))
       ,@(loop for (name lambda-list docs) in sigs
               do (unless lambda-list
                    (error (clojure-syntax-error "Protocol function cannot be nullary.")))
               collect `(defgeneric-1 ,name ,lambda-list
                          ,@(unsplice (and docs `(:documentation ,docs)))))
       (define-symbol-macro ,name (symbol-protocol ',name)))))

(define-clojure-macro #_defprotocol (name &body specs)
  (mvlet* ((specs docs (body+docs+attrs specs))
           (sigs
            (collecting
              (dolist (spec specs)
                (ematch spec
                  ((list* name sigs)
                   (receive (sigs docs)
                       (if (stringp (lastcar sigs))
                           (let ((docs (lastcar sigs))
                                 (sigs (butlast sigs)))
                             (values sigs docs))
                           (values sigs nil))
                     (assert (notany #'fset:empty? sigs))
                     (collect (list name '(x &rest args) docs)))))))))
    `(defprotocol ,name
       ,@(unsplice docs)
       ,@sigs)))

(defprotocol #_Object
  (#_toString (o)))

(defprotocol #_IEquiv
  (#_equiv (self other)))

(defprotocol #_ICollection
  (#_conj (coll x)))

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
  (#_nth (seq n &optional not-found)))

(defun indexed? (x)
  (satisfies? '#_IIndexed x))

(deftype indexed ()
  '(satisfies indexed?))

(defprotocol #_Fn)

(defun-1 #_fn? (x)
  (#_satisfies? '#_Fn x))

(defprotocol #_IFn
  (#_invoke (x &rest args)))

(defun ifncall (ifn &rest args)
  ;; TODO avoid consing.
  (apply (ifn-function ifn) args))

(defun ifn-apply (ifn &rest args)
  (apply #'apply (ifn-function ifn) args))

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

(defun #_ifn? (x)
  (satisfies? '#_IFn x))

(defprotocol #_ISeqable
  (#_seq (x)))

(defprotocol #_IEmptyableCollection
  (#_empty (coll) "Create an empty collection."))

(defprotocol #_IHash
  (#_hash (x)))

(defprotocol #_ILookup
  (#_lookup (obj key &optional not-found)))

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
  (satisfies? '#_ISequential x))

(defprotocol #_IAssociative
  (#_contains-key? (coll k))
  (#_assoc (coll k v)))

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
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (p . methods) in specs
               do (check-protocol p methods)
               collect `(progn
                          (defmethod #_satisfies? ((protocol (eql ',p)) (x ,type))
                            #_true)
                          ,@(loop for (fn-name lambda-list . body) in methods
                                  for this = (first lambda-list)
                                  collect `(defmethod ,fn-name ((,this ,type)
                                                                ,@(rest lambda-list))
                                             ,@body)))))))

(define-clojure-macro #_extend-type (type &body specs)
  (with-unique-names (this args)
    (let ((specs (split-specs specs)))
      `(extend-type ,type
         ,@(loop for (p . methods) in specs
                 append (cons p
                              (loop for (fn-name . body) in methods
                                    collect `(,fn-name (,this &rest ,args)
                                                       (apply (fn ,@body) ,this ,args)))))))))

(defmacro extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(extend-type ,type ,p ,@methods)))))

(define-clojure-macro #_extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(#_extend-type ,type ,p ,@methods)))))

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
       (defun-1 ,constructor-name ,fields
         (make-instance ',type
                        ,@(loop for field in fields
                                append `(',field ,field))))
       (defmethod print-object ((self ,type) stream)
         (print-unreadable-object (self stream :type t)
           (with-slots ,fields self
             (format stream "~{~a~^ ~}"
                     (list ,@fields)))))
       ,@(loop for (protocol-name . methods) in specs
               do (check-protocol protocol-name methods)
               append (loop for (fn-name arg-seq . body) in methods
                            for lambda-list = (seq->lambda-list arg-seq)
                            for this = (first lambda-list)
                            collect `(defmethod ,fn-name ((,this ,type) ,@(rest lambda-list))
                                       (with-slots ,fields ,this
                                         ,@body)))))))

(extend-type t
  #_Object
  (#_toString (x) (princ-to-string x)))

(defun-1 #_str (&rest args)
  (with-output-to-string (s)
    (dolist (arg args)
      (unless (nil? arg)
        (write-string (#_toString arg) s)))))

(extend-type t
  #_IComparable
  (#_compare
   (x y)
   (ecase (fset:compare x y)
     (:greater 1)
     (:less -1)
     (:equal 0)))
  #_INext
  (#_next (x) (#_seq (#_rest x))))

(extend-type function
  #_Fn
  #_IFn
  (#_invoke (fn &rest args) (ifn-apply fn args)))

(extend-protocol #_IHash
  t
  (#_hash (x) (murmurhash:murmurhash x)))

(extend-protocol #_ILookup
  map
  (#_lookup (map key &optional (not-found #_nil))
            (multiple-value-bind (val val?)
                (fset:lookup map key)
              (if val? val not-found)))
  hash-table
  (#_lookup (table key &optional (not-found #_nil))
            (gethash table key not-found)))

(extend-protocol #_IEquiv
  t
  (#_equiv (self other) (? (equal? self other))))

(extend-protocol #_IMeta
  t
  (#_meta (x) (meta x)))

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
  (satisfies? '#_ICounted x))

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
  (#_lookup (coll key &optional (default #_nil))
            (declare (ignore coll key))
            default))

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
  (#_pop (c) (error (clojure-program-error "Pop on empty seq!")))
  #_ILookup
  (#_lookup (coll key &optional (default #_nil))
            (declare (ignore coll key))
            default)
  #_IEquiv
  (#_equiv (n x) (or (null x) (#_empty? x))))

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
           (when (equal self other)
             (return-from #_equiv #_true))
           (if (seq? other)
               (#_and
                (#_equiv (car self) (#_first other))
                (#_equiv (cdr self) (#_rest other)))
               #_false))
  #_ILookup
  (#_lookup (coll key &optional (default #_nil))
            (nlet rec ((tail coll)
                       (i key))
              (if (not (plusp i))
                  (if tail (first tail)
                      default)
                  (rec (rest tail)
                       (1- i))))))

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
  (#_nth (v n &optional (not-found nil not-found-supplied?))
         (if (and (>= n (length v))
                  not-found-supplied?)
             not-found
             (aref v n)))
  #_ILookup
  (#_lookup (coll key &optional (default #_nil))
            (#_nth coll key default))
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
  (#_invoke (x &rest args) (elt x (only-elt args)))
  #_IReversible
  (#_rseq (x) (reverse x))
  #_IIndexed
  (#_nth (v n &optional (not-found nil not-found-supplied?))
         (if (and (>= n (length v))
                  not-found-supplied?)
             not-found
             (elt v n)))
  #_ILookup
  (#_lookup (coll key &optional (default #_nil))
            (#_nth coll key default)))

(extend-type string
  #_IEquiv
  (#_equiv (x y)
           (? (and (stringp y)
                   (string= x y)))))

(extend-type fset:seq
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
  (#_invoke (x &rest args) (fset:lookup x (only-elt args)))
  #_IReversible
  (#_rseq (x) (fset:reverse x))
  #_IIndexed
  (#_nth (v n &optional (not-found nil not-found-supplied?))
         (if (and (>= n (size v))
                  not-found-supplied?)
             not-found
             (fset:lookup v n)))
  #_IStack
  (#_peek (c) (if (empty? c) #_nil
                  (lookup c (1- (size c)))))
  (#_pop (c) (if (empty? c) (error (clojure-program-error "Empty seq"))
                 (fset:subseq c 0 (1- (size c)))))
  #_IAssociative
  (#_contains-key? (seq idx) (< -1 idx (size seq)))
  (#_assoc (seq idx value)
           (if (< idx (size seq))
               (with seq idx value)
               (error (clojure-program-error "Bad index for ~a" seq))))
  #_IKVReduce
  (#_kv-reduce (seq f init)
               (if (empty? seq) seq
                   (let ((k 0))
                     (do-seq (v seq)
                       (setf init (ifncall f init (finc k) v)))
                     init)))
  #_ILookup
  (#_lookup (coll key &optional (default #_nil))
            (#_nth coll key default)))

(extend-type map
  #_ISeqable
  (#_seq (map)
         (if (empty? map) #_nil
             (collecting
               (do-map (k v map)
                 (collect (seq k v))))))
  #_IEmptyableCollection
  (#_empty (map) (fset:empty-map))
  #_ICollection
  (#_conj (map x) (apply #'fset:with map (convert 'list (#_seq x))))
  #_IFn
  (#_invoke (x &rest args) (lookup x (only-elt args)))
  #_IAssociative
  (#_contains-key? (map key) (fset:contains? map key))
  (#_assoc (map key value) (with map key value))
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
  (#_conj (set x) (with set x)))

(defun-1 #_instance? (x class)
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

(defun-1 #_dissoc (map key &rest keys)
  (reduce #'less
          (cons key keys)
          :initial-value map))

(defun-1 #_alter-meta! (obj f &rest args)
  (synchronized (obj)
    (setf (meta obj)
          (ifn-apply f (meta obj) args))))

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
  (multiple-value-bind (val val?)
      (lookup map key)
    (if val? val not-found)))

(defun-1 #_constantly (x)
  (constantly x))

(defun-1 #_alter-var-root! (root f &rest args)
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

(defun-1 #_unchecked-add-int (x y)
  (+ x y))

(defun-1 #_unchecked-multiply-int (x y)
  (- x y))

(define-clojure-macro #_declare (&rest args)
  `(progn
     ,@(loop for arg in args
             collect `(progn
                        (defalias ,arg (constantly #_nil))
                        (#_def arg #_nil)))))

(defun-1 #_= (&rest args)
  (assure clojure-boolean
    (match args
      ((list) #_true)
      ((list _) #_true)
      ((list x y) (#_equiv x y))
      (otherwise
       (? (loop for x in args
                for y in (rest args)
                always (truthy? (#_equiv x y))))))))

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
  `(progn
     (add-clojure-method (symbol-function ',name) ,value
                         (#_fn ,params ,@body))
     ',name))

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

(define-clojure-macro #_if-let (bindings &body (then else))
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
  (? (symbolp x)))

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
      (declare (ignore v))
      (collect k))))

(defun-1 #_keys (map)
  (collecting
    (do-map (k v map)
      (declare (ignore k))
      (collect v))))

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
  (let ((table (make-clojure-hash-table))
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
           (#_= (force self) (force other))))

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
              (fset:map-union map1 map2 fn))
            maps
            :initial-value (empty-map))))

(defun-1 #_merge (&rest maps)
  (apply #_merge-with #'second maps))

(defun-1 #_reduce (f &rest args)
  (let ((f (ifn-function f)))
    (ematch args
      ((list val coll)
       (fset:reduce f coll :initial-value val))
      ((list coll)
       (fset:reduce f coll)))))

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
           (ematch catcher
             ((list* classname name exprs)
              `(,classname (,name) ,@exprs)))))
    (let* ((catchers (find '#_catch forms :key #'car-safe))
           (finallies (find '#_finally forms :key #'car-safe))
           (exprs (remove-if (lambda (form)
                               (or (member form catchers)
                                   (member form finallies)))
                             forms))
           (expr `(progn ,@exprs))
           (expr
             (if finallies
                 `(unwind-protect
                       ,expr
                    ,@(mapcar #'rest finallies))))
           (expr
             (if catchers
                 `(handler-case
                      ,expr
                    ,@(mapcar #'catcher->handler catchers)))))
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

(defmacro #_io! (&body body)
  `(progn
     (when *syncing*
       (error (#_IllegalStateException. "IO in transaction!")))
     ,@body))

(define-clojure-macro #_dosync (&body body)
  `(let ((*sycing* t))
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

(define-clojure-macro #_doseq (binds &body body)
  `(#_doall (#_for ,binds ,@body)))

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

(defun-1 #_subvec (v start &optional end)
  (if (nil? end)
      (fset:subseq v start)
      (fset:subseq v start end)))
