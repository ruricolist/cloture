(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun-1 #_apply-template (argv expr values)
  (let ((argv (convert 'list argv))
        (values (convert 'list values)))
    (assert (= (length argv) (length values)))
    ;; TODO Clojure walk.
    (sublis (mapcar #'cons argv values) expr)))

(define-clojure-macro #_do-template (argv expr &body values)
  (let* ((argv (convert 'list argv))
         (values (convert 'list values))
         (values (batches values (length argv))))
    `(#_do
      ,@(loop for batch in values
              collect (#_apply-template argv expr batch)))))
