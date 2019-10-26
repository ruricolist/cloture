(in-package #:cloture)

(defmethod make-load-form ((seq seq) &optional env)
  (declare (ignore env))
  `(seq ,@(mapcar (op `(quote ,_))
                  (convert 'list seq))))

(defmethod make-load-form ((map map) &optional env)
  (declare (ignore env))
  `(map ,@(collecting
            (do-map (k v map)
              (collect (list `(quote ,k)
                             `(quote ,v)))))))
