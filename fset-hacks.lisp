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

(defmethod make-load-form ((set set) &optional env)
  (declare (ignore env))
  `(set ,@(mapcar (op `(quote ,_))
                  (convert 'list set))))

(defpattern seq (&rest pats)
  (with-unique-names (it)
    `(guard1 (,it :type seq)
             (typep ,it 'seq)
             (size ,it) ,(length pats)
             ,@(loop for pat in pats
                     for i from 0
                     collect `(lookup ,it ,i)
                     collect pat))))
