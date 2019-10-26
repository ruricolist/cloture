(in-package #:cloture)
(in-readtable :standard)

(deftype no-meta ()
  "Type for objects that don't allow metadata."
  '(or string number keyword boolean))

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
    (error "~a cannot have metadata." object))
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
    (string (map (:tag (read x))))))

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
    (withf (meta obj) key value)))

(defun falsy? (x)
  ;; TODO false value?
  (null x))

(defun truthy? (x)
  (not (falsy? x)))

(defun dissect-seq-pattern (pats)
  (mvlet* ((all pats
            (match (last pats 2)
              ((list :|as| all)
               (values all (butlast pats 2)))
              (otherwise (values (string-gensym 'all) pats))))
           (rest pats
            (match (last pats 2)
              ((list '|clojure.core|:& rest)
               (values rest (butlast pats 2)))
              (otherwise (values nil pats)))))
    (values pats rest all)))

(defun fset-seq-pattern (pats)
  (multiple-value-bind (pats rest all)
      (dissect-seq-pattern pats)
    (let ((len (length pats)))
      `(trivia:guard1 (,all :type seq)
                      ;; Missing or excess elements are just bound to nil.
                      (typep ,all 'seq)
                      ,@(loop for pat in pats
                              for i from 0
                              collect `(lookup ,all ,i)
                              collect pat)
                      ,@(and rest
                             `((convert 'list (fset:subseq ,all ,len))
                               ,rest))))))

(defpattern fset-seq (&rest pats)
  (fset-seq-pattern pats))

(defpattern clojuresque-list (&rest pats)
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

(defpattern fset-map (alist)
  (let* ((as (assocdr :|as| alist))
         (alist (remove :|as| alist :key #'car))
         (it (or as (string-gensym 'it))))
    `(guard1 ,it
             (typep ,it 'map)
             ,@(loop for (pat . key) in alist
                     collect `(lookup ,it ,key)
                     collect pat))))

(defun map->alist (map)
  (collecting
    (do-map (k v map)
      (collect (cons k v)))))

;;; TODO
(defun obj->pattern (obj)
  "Convert OBJ into a Trivia destructuring pattern."
  (etypecase obj
    (symbol obj)
    (seq
     (let ((pats (mapcar #'obj->pattern (convert 'list obj))))
       ;; TODO vector, sequence
       `(or (fset-seq ,@pats)
            (clojuresque-list ,@pats))))
    (map
     `(fset-map ,(map->alist obj)))))
