(in-package #:cloture)
(in-readtable clojure-shortcut)

(defclause-sequence in-indexed index-of-indexed
  :access-fn '#_nth
  :size-fn (lambda (v) (#_count v))
  :sequence-type 'indexed
  :element-type t
  :element-doc-string "Elements of an object that extends IIndexed."
  :index-doc-string  "Indices of an object that extends IIndexed.")

;;; These are adapted from the source of Iterate.

(in-package #:iterate)

(defclause-driver (for var on-seq list &optional by (step ''#_rest))
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

(defclause-driver (for var in-seq list &optional by (step ''#_rest))
  "Elements of a seq."
  (top-level-check)
  (let* ((on-var (make-var-and-default-binding 'list :type 'list))
	 (setqs (do-dsetq var `(#_first ,on-var)))
	 (test `(if (not (cloture::seq? ,on-var)) (go ,*loop-end*))))
    (setq *loop-end-used?* t)
    (return-driver-code :initial `((setq ,on-var ,list))
			:next (list test
				    setqs
				    (generate-function-step-code on-var step))
			:variable var)))
