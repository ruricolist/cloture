(in-package #:cloture)
(in-readtable clojure-shortcut)

(defmethod fset:with ((map sy:tree-map) k &optional (v nil v-supplied?))
  (assert v-supplied?)
  (sycamore:tree-map-insert map k v))

(defmethod fset:less ((map sy:tree-map) k &optional (v nil v-supplied?))
  (declare (ignore v))
  (assert (not v-supplied?))
  (sycamore:tree-map-remove map k))

(defmethod fset:with ((set sy:tree-set) k &optional (v nil v-supplied?))
  (declare (ignore v))
  (assert (not v-supplied?))
  (sycamore:tree-set-insert set k))

(defmethod fset:less ((set sy:tree-set) k &optional (v nil v-supplied?))
  (declare (ignore v))
  (assert (not v-supplied?))
  (sycamore:tree-set-remove set k))

(defmethod fset:empty? ((set sy:tree-set))
  (zerop (sycamore:tree-set-count set)))

(defmethod fset:convert ((type (eql 'list))
                         (set sy:tree-set)
                         &key)
  (sycamore:tree-set-list set))

(defmethod fset:empty? ((set sy:tree-map))
  (zerop (sycamore:tree-map-count set)))

(fset:define-cross-type-compare-methods sy:tree-map)
(fset:define-cross-type-compare-methods sy:tree-set)

(defun sycamore-comparator (comparator)
  (lambda (x y)
    (let ((result (funcall comparator x y)))
      (case result
        (#.#_true -1)
        (#.#_false 0)
        (t result)))))

(defun-1 #_sorted-map (&rest keyvals)
  (apply #'#_sorted-map-by #'#_compare keyvals))

(defun-1 #_sorted-map-by (comparator &rest keyvals)
  (let* ((comparator (sycamore-comparator comparator))
         (smap (sy:make-tree-map comparator)))
    (loop for (key val) in (batches keyvals 2 :even t)
          do (setf smap (#_assoc smap key val)))
    smap))

(extend-type sy:tree-map
  #_ICounted
  (#_count (m) (sy:tree-map-count m))
  #_ISeqable
  (#_seq (m)
         (collecting
           (sy:do-tree-map ((k v) m)
             (collect (map-entry k v)))))
  #_ISeq
  (#_first (m) (#_first (#_seq m)))
  (#_rest (m) (#_rest (#_seq m)))
  #_INext
  (#_next (m) (#_next (#_seq m)))
  #_IEmptyableCollection
  (#_empty (m) (sy:empty-tree-map m))
  #_ICollection
  (#_-conj (m x)
           (if (truthy? (#_map? x))
               ;; TODO sycamore merging
               (#_merge m x)
               (destructuring-bind (k v) (convert 'list x)
                 (sy:tree-map-insert m k v))))
  #_IFn
  (#_invoke (m arg) (#_lookup m arg))
  #_ILookup
  (#_lookup (m key) (#_lookup m key #_nil))
  (#_lookup (m key default) (sy:tree-map-find m key default))
  #_IAssociative
  (#_contains-key? (m key) (? (sy:tree-map-contains m key)))
  (#_assoc (m key value) (sy:tree-map-insert m key value))
  #_IMap
  (#_-dissoc (m key keys)
             (#_reduce #'sy:tree-map-remove m (cons key keys)))
  #_IKVReduce
  (#_kv-reduce (m f init)
               (if (zerop (#_count m)) m
                   (iterate (for (k v) in-tree-map m)
                     (reducing-kv k v by f initial-value init))))
  #_IHash
  (#_hash (m) (#_hash-ordered-coll m))
  #_IEquiv
  (#_equiv (self other)
           (? (and (truthy? (#_map? other))
                   (= (#_count self)
                      (#_count other))
                   (iter (for (k1 v1) in-tree-map self)
                     (always (and (truthy? (#_contains? other k1))
                                  (egal v1 (#_lookup other k1)))))))))

(defun-1 #_sorted-set (&rest vals)
  (apply #'#_sorted-set-by #'#_compare vals))

(defun-1 #_sorted-set-by (comparator &rest vals)
  (let* ((comparator (sycamore-comparator comparator))
         (set (sy:make-tree-set comparator)))
    (dolist (v vals set)
      (conjf set v))))

(extend-type sy:tree-set
  #_ISeqable
  (#_seq (set)
         (if (zerop (sy:tree-set-count set)) #_nil
             (sy:tree-set-list set)))
  #_ISeq
  (#_first (set)
           ;; TODO sy:do-tree-set doesn't define a block.
           (block nil
             (sy:do-tree-set (x set)
               (return x))))
  (#_rest (set) (#_rest (#_seq set)))
  #_INext
  (#_next (set)
          (if (zerop (sy:tree-set-count set)) #_nil
              (#_next (#_seq set))))
  ;; TODO Pending pull request.
  ;; #_IEmptyableCollection
  ;; (#_empty (set) (sy:empty-tree-set set))
  #_ICollection
  (#_-conj (set x) (sy:tree-set-insert set x))
  #_IFn
  (#_invoke (set arg) (#_lookup set arg))
  #_IHash
  (#_hash (set) (#_hash-ordered-coll set))
  #_IEquiv
  (#_equiv (self other)
           (? (and (truthy? (#_set? other))
                   (= (#_count self)
                      (#_count other))
                   (iter (for v in-tree-set self)
                     (always (truthy? (#_contains? other v))))))))
