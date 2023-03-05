(in-package #:cloture)
(in-readtable :standard)

(deftype no-meta ()
  "Type for objects that don't allow metadata."
  '(or string number keyword boolean))

(defunion clojure-boolean
  |clojure.core|:|true|
  |clojure.core|:|false|)
;;; Lisp nil is reserved for the empty list.
(defunit |clojure.core|:|nil|)

(fset:define-cross-type-compare-methods |clojure.core|:|nil|)
(fset:define-cross-type-compare-methods |clojure.core|:|true|)
(fset:define-cross-type-compare-methods |clojure.core|:|false|)

(defmethod fset:compare ((self |clojure.core|:|true|) (other |clojure.core|:|true|))
  :equal)
(defmethod fset:compare ((self |clojure.core|:|false|) (other |clojure.core|:|false|))
  :equal)
(defmethod fset:compare ((self |clojure.core|:|nil|) (other |clojure.core|:|nil|))
  :equal)

(defmethod fset:convert ((type (eql 'fset:seq))
                         (x |clojure.core|:|nil|)
                         &key)
  (fset:empty-seq))

(def true |clojure.core|:|true|)
(def false |clojure.core|:|false|)

(defmethod murmurhash ((self |clojure.core|:|true|) &key)
  (murmurhash* '|clojure.core|:|true|))
(defmethod murmurhash ((self |clojure.core|:|false|) &key)
  (murmurhash* '|clojure.core|:|false|))
(defmethod murmurhash ((self |clojure.core|:|nil|) &key)
  (murmurhash* '|clojure.core|:|nil|))

(defmethod fset:convert ((type (eql 'list)) (x |clojure.core|:|nil|) &key)
  '())

(defun ns+name (symbol)
  "Split SYMBOL's name into a namespace and name."
  (let* ((string (symbol-name symbol))
         (pos (position #\/ string)))
    (if (no pos)
        (values nil string)
        (values (nsubseq string pos)
                (nsubseq string (1+ pos))))))

(define-modify-macro withf (&rest item-or-tuple) with)
(define-modify-macro lessf (&rest item-or-tuple) less)

(defvar *meta*
  (tg:make-weak-hash-table
   :test 'eq
   :weakness :key))

(defun meta (object)
  (synchronized (object)
    (values (href *meta* object))))

(defun (setf meta) (value object)
  (when (typep object 'no-meta)
    (error (clojure-error "~a cannot have metadata." object)))
  (synchronized (object)
    (setf (href *meta* object)
          (assure map value))))

(defun with-meta (object meta)
  (setf (meta object) meta)
  object)

(defun ensure-meta (x)
  (etypecase x
    (map x)
    (keyword (map (x true)))
    ;; TODO What should tags be? Strings or symbols?
    (symbol (map (:tag x)))
    (string (map (:tag x)))))

(defun merge-maps (&rest maps)
  (reduce (lambda (m1 m2)
            (let ((out m1))
              (iterate (for (k v) in-map m2)
                (withf out k v)
                (finally (return out)))))
          maps
          :initial-value (empty-map)))

(defun merge-meta! (obj map)
  (setf (meta obj)
        (merge-maps (or (meta obj)
                        (empty-map))
                    map)))

(defun meta-ref (obj key)
  (let ((map (meta obj)))
    (and map (lookup map key))))

(defun (setf meta-ref) (value obj key)
  (synchronized (obj)
    (let ((meta
            (or (meta obj)
                (empty-map))))
      (setf (meta obj)
            (with meta key value)))))

(-> falsy? (t) boolean)
(defun falsy? (x)
  (or (eql x |clojure.core|:|false|)
      (eql x |clojure.core|:|nil|)))

(define-compiler-macro falsy? (&whole call x)
  (select x
    (|clojure.core|:|true| nil)
    (|clojure.core|:|false| t)
    (|clojure.core|:|nil| t)
    (otherwise call)))

(-> truthy? (t) boolean)
(defun truthy? (x)
  (not (falsy? x)))

(define-compiler-macro truthy? (&whole call x)
  (select x
    (|clojure.core|:|true| t)
    (|clojure.core|:|false| nil)
    (|clojure.core|:|nil| nil)
    (otherwise call)))

(defun egal (x y)
  "Are X and Y equal according to Clojure?"
  (truthy? (|clojure.core|:= x y)))

(defun dissect-seq-pattern (pats)
  (mvlet* ((pats (convert 'list pats))
           (all pats
            (match (last pats 2)
              ((list :|as| all)
               (values all (butlast pats 2)))
              (otherwise (values (string-gensym 'all) pats))))
           (rest pats
            (match (last pats 2)
              ((list '|clojure.core|:& rest)
               (values rest (butlast pats 2)))
              (otherwise (values nil pats)))))
    (values pats rest all (length pats))))

(defun seq->lambda-list (seq &key allow-patterns)
  (multiple-value-bind (pats rest all)
      (dissect-seq-pattern (convert 'list seq))
    (assert (not (symbol-package all)))
    (if allow-patterns
        (setf pats
              (loop for pat in pats
                    if (symbolp pat)
                      collect pat
                    else collect (obj->pattern pat))
              rest (obj->pattern rest :rest t))
        (progn
          (assert (every #'symbolp pats))
          (assert (symbolp rest))))
    (append pats
            (and rest (list '&rest rest)))))

(defun safe-elt (seq i)
  (if (>= i (length seq)) |clojure.core|:|nil|
      (elt seq i)))

(defun lookup* (obj x)
  "Lookup X in OBJ, returning Clojure nil if not present."
  (if (typep obj 'sequence) (safe-elt obj x)
      (multiple-value-bind (val val?) (lookup obj x)
        (if val? val |clojure.core|:|nil|))))

(defun build-sequential-pattern (pats)
  (multiple-value-bind (pats rest all len)
      (dissect-seq-pattern pats)
    `(trivia:guard1 ,all
                    ;; Missing or excess elements are just bound to nil.
                    (typep ,all 'indexed)
                    ,@(loop for pat in pats
                            for i from 0
                            collect `(|clojure.core|:|nth| ,all ,i |clojure.core|:|nil|)
                            collect pat)
                    ,@(and rest
                           `((|clojure.core|:|nthrest| ,all ,len)
                             ,rest)))))

(defpattern sequential (&rest pats)
  (build-sequential-pattern pats))

(defpattern clojuresque-list (&rest pats)
  ;; NB This only works for lists with at least as many pats as are
  ;; present in PATS. They still get destructured, but as sequences
  ;; rather than lists, so less efficiently.
  (multiple-value-bind (pats rest all)
      (dissect-seq-pattern pats)
    (let* ((pat
             (if rest
                 `(list* ,@pats ,rest)
                 `(list ,@pats)))
           (pat
             (if all
                 `(and ,all ,pat)
                 pat)))
      pat)))

(defpattern associative (list &key as)
  (let* ((as (or as (string-gensym 'as))))
    `(guard1 ,as
             (typep ,as 'lookupable)
             ,@(loop for (pat key default) in list
                     collect `(|clojure.core|:|lookup| ,as ,key ,default)
                     collect pat))))

(defpattern rest-associative (list &key as)
  (with-unique-names (temp-as)
    (let* ((as (or as (string-gensym 'as))))
      `(guard1 ,temp-as
               (typep ,temp-as 'list)
               (list->map ,temp-as) (guard1 ,as t)
               ,@(loop for (pat key default) in list
                       collect `(|clojure.core|:|lookup| ,as ,key ,default)
                       collect pat)))))

(defun map->alist (map)
  (iterate (for (k v) in-map map)
    (collect (cons k v))))

(defun map->list (map)
  (iterate (for (k v) in-map map)
    (collect k)
    (collect v)))

(defun list->map (l)
  (let ((pairs (batches l 2 :even t)))
    (reduce (lambda (map pair)
              (destructuring-bind (key value) pair
                (fset:with map key value)))
            pairs
            :initial-value (empty-map))))

;;; TODO
(defun obj->pattern (obj &key rest)
  "Convert OBJ into a Trivia destructuring pattern.
Also return (as a second value) a list of all the symbols bound."
  (let ((syms (queue)))
    (labels ((obj->pattern (obj)
               (etypecase obj
                 (keyword obj)
                 ((eql |clojure.core|:&) obj)
                 (symbol
                  (enq obj syms)
                  obj)
                 (seq
                  (let ((pats (mapcar #'obj->pattern (convert 'list obj))))
                    `(or (clojuresque-list ,@pats)
                       ;; NB this matches lists with too few arguments.
                       (sequential ,@pats))))
                 ((cons (eql {}) t)
                  (obj->pattern (list->map (rest obj))))
                 (map
                  (let* ((alist (map->alist obj))
                         (as (cdr (pop-assoc :|as| alist)))
                         (or-map (or (cdr (pop-assoc :|or| alist))
                                     (empty-map)))
                         (or-map
                           (let ((map (empty-map)))
                             (iterate (for (k v) in-map or-map)
                               (withf map (make-keyword k) v)
                               (finally (return map)))))
                         (keys (cdr (pop-assoc :|keys| alist)))
                         (strs (cdr (pop-assoc :|strs| alist)))
                         (syms (cdr (pop-assoc :|syms| alist)))
                         (alist
                           (append
                            (and keys
                                 (loop for key in (convert 'list keys)
                                       collect `(,key . ,(make-keyword key))))
                            (and strs
                                 (loop for str in (convert 'list strs)
                                       collect `(,str . ,(string str))))
                            (and syms
                                 (loop for sym in (convert 'list syms)
                                       collect `(,sym . ',sym)))
                            alist))
                         (list
                           (loop for (obj . key) in alist
                                 for default = (|clojure.core|:|lookup| or-map key)
                                 for pat = (obj->pattern obj)
                                 collect (list pat key default))))
                    (if rest
                        `(rest-associative ,list :as ,as)
                        `(associative ,list :as ,as)))))))
      (values (obj->pattern obj)
              (qlist syms)))))

(defun fbind-keywords (keywords)
  (dolist (keyword (ensure-list keywords))
    (assert (keywordp keyword))
    (unless (fboundp keyword)
      (setf (symbol-function keyword)
            (lambda (map &optional (not-found |clojure.core|:|nil|))
              (|clojure.core|:|get| map keyword not-found))))))

(defun proclaim-keywords (&rest keywords)
  (fbind-keywords keywords))

(defmacro declare-keywords (&rest keywords)
  `(eval-always
     (proclaim-keywords ,@keywords)))

;;; Macro helpers.

(defun body+docs+attrs (body)
  (let ((docs (and (stringp (car body)) (pop body)))
        (attrs (and (typep (car body) 'map) (pop body))))
    (values body docs attrs)))

(defun var (sym &optional env)
  (or (find-var sym env)
      (error (clojure-error "Not a var: ~a" sym))))

(defun find-var (sym &optional env)
  ;; NB We do not look for a specific prefix, because the "var" could
  ;; also be a pre-defined Lisp dynamic variable.
  (let ((exp (macroexpand sym env)))
    (unless (or (eql exp sym)
              (not (symbolp exp)))
      exp)))

(defconstructor protocol
  (name symbol)
  (functions list))

(define-namespace protocol protocol)

(defun check-protocol (protocol-name fns)
  ;; TODO Are protocols supposed to be exhaustive?
  (let* ((protocol (symbol-protocol protocol-name))
         (protocol-fn-names (mapcar #'ensure-car (protocol-functions protocol)))
         (fn-names (mapcar #'ensure-car fns)))
    (assert (subsetp fn-names protocol-fn-names))))

(defun split-specs (specs)
  "Split the common Clojure syntax of a symbol (protocol, type) and a list of protocol/interface implementations."
  (if (emptyp specs) nil
      (runs specs :test (lambda (x y) (declare (ignore x))
                          (not (symbolp y))))))

(defun autogensym? (x)
  (and (symbolp x)
       (not (keywordp x))
       (string$= "#" x)))

(defun autogensyms (tree)
  (let ((table (make-hash-table))
        (tree (declojurize tree)))
    (leaf-map (lambda (tree)
                (match tree
                  ((and sym
                        (type symbol)
                        (not (type keyword))
                        (satisfies (lambda (x) (string$= "#" x))))
                   (ensure2 (href table sym)
                     (string-gensym (slice (string tree) 0 -1))))
                  (otherwise tree)))
              tree)))

(defun egal-hash (x)
  ;; Use sxhash to reduce to the Lisp implementation's range.
  (sxhash (|clojure.core|:|hash| x)))

;;; Hash tables that use Clojure's idea of equality.
(define-custom-hash-table-constructor
    make-egal-hash-table
  :test egal
  :hash-function egal-hash)

(defclass multimethod ()
  ((name :initarg :name)
   (fn :initarg :fn :type function)
   (lock :initform (bt:make-lock) :reader monitor)
   (method-table
    :type hash-table
    :initform (make-egal-hash-table))
   (default-value
    :initarg :default)
   ;; TODO
   (hierarchy
    :initarg :hierarchy))
  (:default-initargs
   :default :|default|
   :fn (error "A multimethods needs a function."))
  (:metaclass funcallable-standard-class))

(defmethod print-object ((self multimethod) stream)
  (with-slots (name fn) self
    (print-unreadable-object (self stream :type t)
      (format stream "~a ~a"
              name fn))))

(defmethod find-method-from-value ((self multimethod) value)
  (with-slots (name method-table default-value) self
    (or (href method-table value)
        (href method-table default-value)
        (error 'no-such-method
               :multi name
               :value value))))

(defmethod dispatch ((self multimethod) args)
  (with-slots (name method-table default-value fn) self
    (let* ((value (ifn-apply fn args))
           (method (find-method-from-value self value)))
      (apply method args))))

(defmethod initialize-instance :after ((self multimethod) &key)
  (with-slots (name method-table default-value fn) self
    (set-funcallable-instance-function
     self
     (lambda (&rest args)
       (dispatch self args)))))

(defmethod add-clojure-method ((self multimethod) value fn)
  (with-slots (method-table) self
    (setf (href method-table value)
          (ifn-function fn))))

(defun extract-pre-post (body)
  (match body
    ((list* (and cond-map (type map))
            body)
     (let ((pre (lookup cond-map :|pre|))
           (post (lookup cond-map :|post|)))
       (if (or pre post)
           (values body pre post)
           (trivia.fail:fail))))
    (otherwise
     (values body nil nil))))

(defstruct-read-only fn-clause
  params exprs pre post rest min-arity)

(defun parse-clause (clause)
  (mvlet* ((params exprs (car+cdr clause))
           (exprs pre post (extract-pre-post exprs))
           (subpats rest all min-arity (dissect-seq-pattern params)))
    (declare (ignore subpats))
    (when (symbol-package all)
      (error (clojure-syntax-error "No :as in fn.")))
    (make-fn-clause :params params
                    :exprs exprs
                    :pre pre
                    :post post
                    :rest rest
                    :min-arity min-arity)))

(defun fn-clause->body (c)
  (with-accessors ((exprs fn-clause-exprs)
                   (pre fn-clause-pre)
                   (post fn-clause-post))
      c
    (let* ((exprs
             (if pre
                 `((|clojure.core|:|do| (|clojure.core|:|assert| ,pre) ,@exprs))
                 exprs))
           (exprs
             (if post
                 (let ((% (intern "%")))
                   `((|clojure.core|:|let| ,(seq % `(|clojure.core|:|do| ,@exprs))
                                    (|clojure.core|:|assert| ,post)
                                    ,%)))
                 exprs)))
      exprs)))

(defun symbol-drop-leading-dash (symbol)
  (if (string^= "-" symbol)
      (intern (drop 1 (string symbol))
              (symbol-package symbol))
      symbol))
