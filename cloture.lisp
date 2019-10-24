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

;;; TODO
(defun obj->pattern (obj)
  "Convert OBJ into a Trivia destructuring pattern."
  (etypecase obj
    (symbol obj)
    (seq `(sequence ,@(convert 'list obj))))
  obj)

(defun read-clojure (stream
                     &key (eof-error-p t)
                          eof-value
                          recursive)
  (let ((*readtable* (find-readtable 'cloture))
        (*package* (find-package "user"))
        (*read-default-float-format* 'double-float))
    (read stream eof-error-p eof-value recursive)))
