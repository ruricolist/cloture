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

(defmethod murmurhash:murmurhash ((self |clojure.core|:|true|) &key)
  (murmurhash:murmurhash '|clojure.core|:|true|))
(defmethod murmurhash:murmurhash ((self |clojure.core|:|false|) &key)
  (murmurhash:murmurhash '|clojure.core|:|false|))
(defmethod murmurhash:murmurhash ((self |clojure.core|:|nil|) &key)
  (murmurhash:murmurhash '|clojure.core|:|nil|))

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
    (keyword (map (x t)))
    ;; TODO What should tags be? Strings or symbols?
    (symbol (map (:tag x)))
    (string (map (:tag x)))))

(defun merge-maps (&rest maps)
  (reduce (lambda (m1 m2)
            (let ((out m1))
              (do-map (k v m2 out)
                (withf out k v))))
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

(defun falsy? (x)
  ;; TODO false value?
  (or (eql x |clojure.core|:|false|)
      (eql x |clojure.core|:|nil|)))

(defun truthy? (x)
  (not (falsy? x)))

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

(defun seq->lambda-list (seq)
  (multiple-value-bind (pats rest all)
      (dissect-seq-pattern (convert 'list seq))
    (assert (not (symbol-package all)))
    (assert (every #'symbolp pats))
    (assert (symbolp rest))
    (append pats
            (and rest (list '|clojure.core|:&)))))

(defun lookup* (obj x)
  "Lookup X in OBJ, returning Clojure nil if not present."
  (multiple-value-bind (val val?) (lookup obj x)
    (if val? val |clojure.core|:|nil|)))

(defun fset-seq-pattern (pats)
  (multiple-value-bind (pats rest all len)
      (dissect-seq-pattern pats)
    `(trivia:guard1 (,all :type seq)
                    ;; Missing or excess elements are just bound to nil.
                    (typep ,all 'seq)
                    ,@(loop for pat in pats
                            for i from 0
                            collect `(lookup* ,all ,i)
                            collect pat)
                    ,@(and rest
                        `((convert 'list (fset:subseq ,all ,len))
                          ,rest)))))

(defpattern fset-seq (&rest pats)
  (fset-seq-pattern pats))

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

(defun safe-elt (seq i)
  (if (>= i (length seq)) |clojure.core|:|nil|
      (elt seq i)))

(defpattern clojuresque-sequence (&rest pats)
  (multiple-value-bind (pats rest all len)
      (dissect-seq-pattern pats)
    `(trivia:guard1 (,all :type sequence)
                    (typep ,all 'sequence)
                    ,@(loop for pat in pats
                            for i from 0
                            collect `(safe-elt ,all ,i)
                            collect pat)
                    ,@(and rest
                           `((drop ,len ,all)
                             ,rest)))))

(defpattern fset-map (alist)
  (let* ((as (assocdr :|as| alist))
         (alist (remove :|as| alist :key #'car))
         (it (or as (string-gensym 'it))))
    `(guard1 ,it
             (typep ,it 'map)
             ,@(loop for (pat . key) in alist
                     collect `(lookup* ,it ,key)
                     collect pat))))

(defun map->alist (map)
  (collecting
    (do-map (k v map)
      (collect (cons k v)))))

(defun map->list (map)
  (collecting
    (do-map (k v map)
      (collect k v))))

(defun list->map (l)
  (let ((pairs (batches l 2 :even t)))
    (reduce (lambda (map pair)
              (destructuring-bind (key value) pair
                (fset:with map key value)))
            pairs
            :initial-value (empty-map))))

;;; TODO
(defun obj->pattern (obj)
  "Convert OBJ into a Trivia destructuring pattern.
Also return (as a second value) a list of all the symbols bound."
  (let ((syms (queue)))
    (labels ((map->alist (map)
               (collecting
                 (do-map (k v map)
                   (enq v syms)
                   (collect (cons k v)))))
             (obj->pattern (obj)
               (etypecase obj
                 (keyword obj)
                 (symbol
                  (enq obj syms)
                  obj)
                 (seq
                  (let ((pats (mapcar #'obj->pattern (convert 'list obj))))
                    ;; TODO vector, sequence
                    `(or (fset-seq ,@pats)
                         (clojuresque-list ,@pats)
                         ;; NB this matches lists with too few arguments.
                         (clojuresque-sequence ,@pats))))
                 (map
                  `(fset-map ,(map->alist obj))))))
      (values (obj->pattern obj)
              (qlist syms)))))

(defun fbind-keywords (keywords)
  (dolist (keyword (ensure-list keywords))
    (assert (keywordp keyword))
    (unless (fboundp keyword)
      (setf (symbol-function keyword)
            (lambda (map)
              (|clojure.core|:|get| map keyword))))))

;;; Macro helpers.

(defun body+docs+attrs (body)
  (let ((docs (and (stringp (car body)) (pop body)))
        (attrs (and (typep (car body) 'map) (pop body))))
    (values body docs attrs)))

(defun car+cdr (list)
  (values (car list) (cdr list)))

(defun var (sym &optional env)
  (let ((exp (macroexpand-1 (assure symbol sym) env)))
    (when (or (eql exp sym)
              (not (symbolp exp))
              (not (meta-ref sym :|dynamic|)))
      (error (clojure-error "Not a var: ~a" sym)))
    exp))

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

(defun declojurize (tree)
  "Replace literal objects (outside quasiquotes) with constructors."
  (map-tree (named-lambda rec (tree)
              (match tree
                ((type seq)
                 `([]
                   ,@(mapcar (op (map-tree #'rec _))
                             (convert 'list tree))))
                ((type set)
                 `(|#{}|
                   ,@(mapcar (op (map-tree #'rec _))
                             (convert 'list tree))))
                ((type map)
                 `({}
                   ,@(mapcar (op (map-tree #'rec _))
                             (map->list tree))))
                (otherwise tree)))
            tree))

(defun clojurize (tree)
  "Replace calls to constructors with literal objects.
Also convert the symbols for true, false, and nil to unit types."
  (map-tree (named-lambda rec (tree)
              (match tree
                ((list* '[] elts)
                 (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                   (convert 'seq elts)))
                ((list* '|#{}| elts)
                 (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                   (convert 'set elts)))
                ((list* '{} elts)
                 (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                   (list->map elts)))
                ('|clojure.core|:|true|  |clojure.core|:|true|)
                ('|clojure.core|:|false| |clojure.core|:|false|)
                ('|clojure.core|:|nil|   |clojure.core|:|nil|)
                ((list* _ (and _ (not (type list))))
                 (error "Improper list in Clojure tree."))
                (otherwise tree)))
            tree))

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

(defun hash-clojuresque (x)
  ;; Use sxhash to reduce to the Lisp implementation's range.
  (sxhash (|clojure.core|:|hash| x)))

;;; Hash tables that use Clojure's idea of equality.
(define-custom-hash-table-constructor
    make-clojure-hash-table
  :test |clojure.core|:|=|
  :hash-function hash-clojuresque)

(defclass multimethod ()
  ((name :initarg :name)
   (fn :initarg :fn :type function)
   (lock :initform (bt:make-lock) :reader monitor)
   (method-table
    :type hash-table
    :initform (make-clojure-hash-table))
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

(defmethod dispatch ((self multimethod) args)
  (with-slots (name method-table default-value fn) self
    (let ((value (ifn-apply fn args)))
      (if-let (method (href method-table value))
        (apply method args)
        (if-let (default-method (href method-table default-value))
          (apply default-method args)
          (error 'no-such-method
                 :multi name
                 :value value))))))

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
