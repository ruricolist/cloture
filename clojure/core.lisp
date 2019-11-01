(in-package #:cloture)
(in-readtable clojure-shortcut)

;;; Note that there are many special cases here that could be compiled more efficiently or inlined, or macro-expanded more legibly. For now simplicity & uniformity is the goal. In the future, maybe, when there is more code to test against, optimization might be worthwile. Not yet.

(defconst special-forms
  '#_(quote
      if do def let binding var
      loop recur throw try))

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

(defun-1 #_nil? (x)
  (eql x #_nil))

(defun-1 #_false? (x)
  (eql x #_false))

(defun-1 #_true? (x)
  (eql x #_true))

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
            `(export ',name ,(package-name (symbol-package name)))))
       ',name)))

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

(defmacro alias-from (from to)
  `(progn
     (defmacro ,to (&body body)
       (cons ',from body))
     (define-symbol-macro ,to ,from)))

(defun qualify-symbol (ns symbol)
  (intern (string+ ns "/" symbol)))

(defun-1 #_refer-clojure (&rest args)
  (apply #'#_refer :|clojure.core| args))

(defun setup-qualified-names (p &optional prefix)
  (let* ((package (find-package p))
         (prefix (or prefix (package-name package))))
    (dolist (export (package-exports package))
      (let ((qname (qualify-symbol prefix export)))
        (eval `(alias-from ,export ,qname))))))

(defun-1 #_refer (ns &key exclude only rename)
  (let ((p (find-package ns)))
    (setup-qualified-names p)
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
                (eval `(alias-from ,from ,to)))))))))

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
           (let ((name (string+ prefix libspec)))
             (expect-package name)
             (setup-qualified-names name)))
          ((and libspec (type seq))
           (ematch (convert 'list libspec)
             ((lambda-list lib &key as refer exclude only rename)
              (when (and (or exclude only rename) refer)
                (error "Invalid: ~a" libspec))
              (let ((lib (string+ prefix lib)))
                (expect-package lib)
                (setup-qualified-names lib as)
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
                          `(#_require ,@args))
                         ((list* :|refer-clojure| args)
                          `(#_refer-clojure ,@args))
                         ((list* :|refer-cl| args)
                          `(#_refer-cl ,@args))))
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

(defmacro #_when-not (test &body body)
  `(#_if-not ,test (#_do ,@body)))

(defun-1 #_apply (fn &rest args)
  (apply #'apply fn args))

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

(defmacro defprotocol (name &body specs)
  (mvlet ((doc sigs (parse-docs specs)))
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
                    (error "Protocol function cannot be nullary."))
               collect `(defgeneric-1 ,name ,lambda-list
                          ,@(unsplice (and docs `(:documentation ,docs)))))
       (define-symbol-macro ,name (symbol-protocol ',name)))))

(defmacro #_defprotocol (name &body specs)
  (mvlet* ((docs specs (parse-docs specs))
           (sigs
            (collecting
              (dolist (spec specs)
                (ematch spec
                  ((lambda-list name
                                (and args (type seq))
                                &optional docs)
                   (let ((lambda-list (seq->lambda-list args)))
                     (collect (list name lambda-list docs)))))))))
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

(defprotocol #_INext
  (#_next (seq)))

(defprotocol #_IFn
  (#_invoke (x &rest args)))

(defprotocol #_ISeqable
  (#_seq (x)))

(defprotocol #_IEmptyableCollection
  (#_empty (coll) "Create an empty collection."))

(defprotocol #_IHash
  (#_hash (x)))

(defprotocol #_ILookup
  (#_lookup (obj key &optional not-found)))

(defprotocol #_ICounted
  (#_count (col)))

(defprotocol #_IReversible
  (#_rseq (col)))

(defprotocol #_ISequential)

(defprotocol #_IAssociative
  (#_contains? (coll k))
  (#_assoc (coll k v)))

(defprotocol #_IComparable
  (#_compare (x y)))

(defprotocol #_IMeta
  (#_meta (x)))

(defprotocol #_IWithMeta
  (#_with-meta (o meta)))

(defgeneric extends? (protocol-name type))

(defgeneric-1 #_extends? (protocol atype)
  (:method ((protocol protocol) atype)
    (extends? (protocol-name protocol) atype)))
(defgeneric-1 #_satisfies? (protocol x))

(defmacro extend-type (type &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (p . methods) in specs
               do (check-protocol p methods)
               collect `(progn
                          (defmethod extends? ((protocol (eql ,p)) (x ,type))
                            t)
                          ,@(loop for (fn-name lambda-list . body) in methods
                                  for this = (first lambda-list)
                                  collect `(defmethod ,fn-name ((,this ,type)
                                                                ,@(rest lambda-list))
                                             ,@body)))))))

(defmacro #_extend-type (type &body specs)
  (let ((specs (split-specs specs)))
    `(extend-type ,type
       ,@(loop for (p . methods) in specs
               append (cons p
                            (loop for (fn-name arg-seq . body) in methods
                                  for lambda-list = (seq->lambda-list arg-seq)
                                  collect (list* fn-name lambda-list body)))))))

(defmacro extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(extend-type ,type ,p ,@methods)))))

(defmacro #_extend-protocol (p &body specs)
  (let ((specs (split-specs specs)))
    `(progn
       ,@(loop for (type . methods) in specs
               collect `(#_extend-type ,type ,p ,@methods)))))

(defclass clojure-class ()
  ())

(defmacro #_deftype (type fields &body opts+specs)
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
      (unless (#_nil? arg)
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

(extend-protocol #_IFn
  function
  (#_invoke (fn &rest args) (apply fn args)))

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
  (#_equiv (self other) (equal? self other)))

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
  (extends? x #_ICounted))

(extend-type #_nil
  #_ISeq
  (#_first (x) #_nil)
  (#_rest (x) nil)
  #_ISeqable
  (#_seq (x) #_nil)
  #_IEmptyableCollection
  (#_empty (x) #_nil)
  #_ICollection
  (#_conj (coll x) (list x)))

(extend-type null
  #_ISeq
  (#_first (x) #_nil)
  (#_rest (x) nil)
  #_ISeqable
  (#_seq (x) #_nil)
  #_IEmptyableCollection
  (#_empty (x) nil)
  #_ICollection
  (#_conj (coll x) (list x)))

(extend-type cons
  #_ISeq
  (#_first (x) (car x))
  (#_rest (x) (cdr x))
  #_ISeqable
  (#_seq (x) x)
  #_IEmptyableCollection
  (#_empty (x) nil)
  #_ICollection
  (#_conj (coll x) (cons x coll)))

;; Vectors (and strings).
(extend-type vector
  #_ISeq
  (#_first (x) (if (emptyp x) #_nil (aref x 0)))
  (#_rest (x) (if (emptyp x) () (nsubseq x 1)))
  #_ISeqable
  (#_seq (x) (if (emptyp x) #_nil x))
  #_IEmptyableCollection
  (#_empty (x) #()))

(extend-type sequence
  #_ISeq
  (#_first (x) (if (emptyp x) #_nil (elt x 0)))
  (#_rest (x) (if (emptyp x) () (subseq x 1)))
  #_ISeqable
  (#_seq (seq) (if (emptyp seq) #_nil seq))
  #_IEmptyableCollection
  (#_empty (x) (subseq x 0 0)))

(extend-type fset:seq
  #_ISeq
  (#_first (x) (if (empty? x) #_nil (fset:first x)))
  (#_rest (x) (if (empty? x) () (fset:subseq x 1)))
  #_ISeqable
  (#_seq (seq) (if (empty? seq) #_nil seq))
  #_IEmptyableCollection
  (#_empty (seq) (fset:empty-seq))
  #_ICollection
  (#_conj (seq x) (fset:with-last seq x)))

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
  (#_conj (map x) (apply #'fset:with map (convert 'list (#_seq x)))))

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

(defmacro #_new (class-name &rest args)
  `(make-instance ',class-name ,@args))

(defun-1 #_instance? (x class)
  (typep x class))

(defun-1 #_type (x)
  (type-of x))

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
