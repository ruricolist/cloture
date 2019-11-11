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

(defmethod murmurhash:murmurhash ((seq seq) &key (seed murmurhash:*default-seed*)
                                                 mix-only)
  (murmurhash:murmurhash
   (list* '%seq (size seq) (convert 'list seq))
   :seed seed :mix-only mix-only))

(defmethod murmurhash:murmurhash ((set set) &key (seed murmurhash:*default-seed*)
                                                 mix-only)
  (murmurhash:murmurhash
   (list* '%set (size set) (convert 'list set))
   :seed seed :mix-only mix-only))

(defmethod murmurhash:murmurhash ((map map) &key (seed murmurhash:*default-seed*)
                                                 mix-only)
  (murmurhash:murmurhash
   (list* '%set (size map) (map->alist map))
   :seed seed :mix-only mix-only))
