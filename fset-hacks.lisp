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

(defmethod murmurhash ((seq seq) &key (seed *default-seed*)
                                      mix-only)
  (murmurhash
   (list* '%seq (size seq) (convert 'list seq))
   :seed seed :mix-only mix-only))

(defmethod murmurhash ((set set) &key (seed *default-seed*)
                                      mix-only)
  (murmurhash
   (list* '%set (size set) (convert 'list set))
   :seed seed :mix-only mix-only))

(defmethod murmurhash ((map map) &key (seed *default-seed*)
                                      mix-only)
  (murmurhash
   (list* '%set (size map) (map->alist map))
   :seed seed :mix-only mix-only))

;;; We want to be able to build on FSet's idea of equality, but we
;;; also need FSet to take into account Clojure's idea of equality (so
;;; that maps have the correct behavior). The following hack lets that
;;; work by detecting and breaking recursion.

(defmethod fset:compare (a b)
  (handler-case
      (without-recursion ()
        (if (truthy? (|clojure.core|:|=| a b)) :equal :unequal))
    (recursion-forbidden ()
      (call-next-method))))
