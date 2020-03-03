(in-package #:cloture)

(defclause-sequence in-indexed index-of-indexed
  :access-fn '|clojure.core|:|nth|
  :size-fn (lambda (v) (|clojure.core|:|count| v))
  :sequence-type 'indexed
  :element-type t
  :element-doc-string "Elements of an object that extends IIndexed."
  :index-doc-string  "Indices of an object that extends IIndexed.")

(defclause-sequence cloture:in-fset-seq cloture:index-of-fset-seq
  :access-fn 'fset:lookup
  :size-fn 'fset:size
  :sequence-type 'fset:seq
  :element-type t
  :element-doc-string "Elements of an Fset seq."
  :element-doc-string "Indices of an Fset seq.")

(defmacro-driver (for x in-set s)
  (with-unique-names (gset elt)
    (let ((kwd (if generate 'iter:generate 'iter:for)))
      `(progn
         (iterate:with ,gset = ,s)
         (,kwd ,x
               next
               (if (empty? ,gset)
                   (terminate)
                   (let ((,elt (fset:arb ,gset)))
                     (setf ,gset (fset:less ,gset ,elt))
                     ,elt)))))))

(defmacro-driver (for x in-tree-set s)
  (with-unique-names (gset elt temp)
    (let ((kwd (if generate 'iter:generate 'iter:for)))
      `(progn
         (iterate:with ,gset = ,s)
         (,kwd ,x
               next
               (if (= 0 (sycamore:tree-set-count ,gset))
                   (terminate)
                   (multiple-value-bind (,temp ,elt)
                       (sycamore:tree-set-remove-min ,gset)
                     ,elt)))))))

(defun tree-map-min (m)
  (sy:do-tree-map ((k v) m)
    (return-from tree-map-min
      (values k v))))

(defmacro-clause (for var in-plist plist)
  (destructuring-bind (k &optional v) var
    `(for (,k ,@(unsplice v))
       on ,plist by #'cddr)))

(defmacro with-fset-iterator ((name col) &body body)
  `(fbind ((,name (curry (fset:iterator ,col) :get)))
     ,@body))

(defmacro-clause (collecting-set x &optional into var)
  `(reducing ,x by #'fset:with into ,var initial-value (empty-set)))

(defmacro-clause (collecting-seq x &optional into var)
  `(reducing ,x by #'fset:with-last into ,var initial-value (empty-seq)))

(defun with-kv (map kv)
  (destructuring-bind (k . v) kv
    (fset:with map k v)))

(defmacro-clause (collecting-map-aux kv &optional into var)
  `(reducing ,kv by #'with-kv into ,var initial-value (empty-map)))

(defmacro collecting-map (k v &rest args)
  `(collecting-map-aux (cons ,k ,v) ,@args))

(defmacro reducing-kv (k v by f &rest args)
  (assert (string-equal by 'by))
  (with-unique-names (fn)
    `(let ((,fn (ifn-function ,f)))
       (reducing (cons ,k ,v) by (lambda (old kv)
                                   (funcall ,fn old (car kv) (cdr kv)))
                 ,@args))))

(iterate::defclause-driver (for key-val-vars in-map map)
  "Elements and keys of an Fset map."
  (iterate::top-level-check)
  (unless (consp key-val-vars)
    (iterate::clause-error "~a should be a list of up to two variables: the first ~
  for the keys, the second for the values." key-val-vars))
  (let* ((iterator (gensym "FSET-ITERATOR-"))
         (more?    (gensym))
         (var-spec `(values ,@key-val-vars ,more?))
         (setqs    (iterate::do-dsetq var-spec `(,iterator)))
         (test     `(and (not ,more?) (go ,iterate::*loop-end*))))
    (setf iterate::*loop-end-used?* t)
    (iterate::add-loop-body-wrapper `(with-fset-iterator (,iterator ,map)))
    (iterate::return-driver-code :next (list setqs test)
                                 :variable var-spec)))

(defun tree-map-iterator (map)
  (lambda ()
    (let ((len (sycamore:tree-map-count map)))
      (if (zerop len)
          (values nil nil nil)
          (multiple-value-bind (k v) (tree-map-min map)
            (setf map (sy:tree-map-remove map k))
            (values k v t))))))

(defmacro with-tree-map-iterator ((name col) &body body)
  `(fbind ((,name (tree-map-iterator ,col)))
     ,@body))

(iterate::defclause-driver (for key-val-vars in-tree-map map)
  "Elements and keys of a Sycamore map."
  (iterate::top-level-check)
  (unless (consp key-val-vars)
    (iterate::clause-error "~a should be a list of up to two variables: the first ~
  for the keys, the second for the values." key-val-vars))
  (let* ((iterator (gensym "SY-ITERATOR-"))
         (more?    (gensym))
         (var-spec `(values ,@key-val-vars ,more?))
         (setqs    (iterate::do-dsetq var-spec `(,iterator)))
         (test     `(and (not ,more?) (go ,iterate::*loop-end*))))
    (setf iterate::*loop-end-used?* t)
    (iterate::add-loop-body-wrapper `(with-tree-map-iterator (,iterator ,map)))
    (iterate::return-driver-code :next (list setqs test)
                                 :variable var-spec)))

;;; These are adapted from the source of Iterate.

(in-package #:iterate)

(defclause-driver (for var cloture:on-seq list &optional by (step ''|clojure.core|:|rest|))
  "Rests of a seq."
  (top-level-check)
  (let* ((list-var (make-var-and-default-binding 'list))
	 ;; Handle dotted lists, so type declaration is not possible
	 (setqs (do-dsetq var list-var t 'list))
	 (test `(if (not (cloture::seq? ,list-var)) (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (return-driver-code :initial `((setq ,list-var ,list))
			:next (list test
				    setqs
				    (generate-function-step-code
				     list-var step))
			:variable var)))

(defclause-driver (for var cloture:in-seq list &optional by (step ''|clojure.core|:|rest|))
  "Elements of a seq."
  (top-level-check)
  (let* ((on-var (make-var-and-default-binding 'list :type 'list))
	 (setqs (do-dsetq var `(|clojure.core|:|first| ,on-var)))
	 (test `(if (not (cloture::seq? ,on-var)) (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (return-driver-code :initial `((setq ,on-var ,list))
			:next (list test
				    setqs
				    (generate-function-step-code on-var step))
			:variable var)))
