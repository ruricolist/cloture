(in-package #:cloture)

(defconst int-length 32)

(defconst long-length 64)

(deftype int ()
  '(signed-byte #.int-length))

(deftype long ()
  '(signed-byte #.long-length))

(defsubst mask-signed-field (size int)
  #+sbcl (sb-c::mask-signed-field size int)
  #-sbcl
  (cond ((zerop size)
         0)
        ((logbitp (1- size) int)
         (dpb int (byte size 0) -1))
        (t
         (ldb (byte size 0) int))))

(defsubst mask-int (int)
  (mask-signed-field int-length int))
(defsubst mask-long (int)
  (mask-signed-field long-length int))

(defpattern seq (&rest pats)
  (with-unique-names (it)
    `(guard1 (,it :type seq)
             (typep ,it 'seq)
             (size ,it) ,(length pats)
             ,@(loop for pat in pats
                     for i from 0
                     collect `(lookup ,it ,i)
                     collect pat))))

(defun murmurhash* (x)
  (mask-int (murmurhash x)))

(defmethod murmurhash ((seq seq) &key (seed *default-seed*)
                                      mix-only)
  (mask-int
   (murmurhash
    (list* '%seq (size seq) (convert 'list seq))
    :seed seed :mix-only mix-only)))

(defmethod murmurhash ((set set) &key (seed *default-seed*)
                                      mix-only)
  (mask-int
   (murmurhash
    (list* '%set (size set) (convert 'list set))
    :seed seed :mix-only mix-only)))

(defmethod murmurhash ((map map) &key (seed *default-seed*)
                                      mix-only)
  (mask-int
   (murmurhash
    (list* '%set (size map) (map->alist map))
    :seed seed :mix-only mix-only)))

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

(defmethod fset:compare :around ((a symbol) (b symbol))
  (flet ((compare-by-name (a b)
           (let ((name1 (symbol-name a))
                 (name2 (symbol-name b)))
             (eif (or (find #\/ name1)
                      (find #\/ name2))
                 (call-next-method)
                 (if (equal name1 name2)
                     :equal
                     (call-next-method))))))
    (eif (keywordp a)
        (call-next-method)
        (eif (keywordp b)
            (call-next-method)
            (let ((package1 (symbol-package a)))
              (eif (clojure-package? package1)
                  (let ((package2 (symbol-package b)))
                    (eif (clojure-package? package2)
                        (compare-by-name a b)
                        (call-next-method)))
                  (call-next-method)))))))
