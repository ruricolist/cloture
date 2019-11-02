(in-package #:cloture)
(in-readtable :standard)

(deftype no-meta ()
  "Type for objects that don't allow metadata."
  '(or string number keyword boolean))

(defunit |clojure.core|:|true|)
(defunit |clojure.core|:|false|)
;;; Lisp nil is reserved for the empty list.
(defunit |clojure.core|:|nil|)

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
    (string (map (:tag x)))))

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
    (let ((meta
            (or (meta obj)
                (empty-map))))
      (setf (meta obj)
            (with meta key value)))))

(defun falsy? (x)
  ;; TODO false value?
  (or (eql x |clojure.core|:|false|)
      (eql x |clojure.core|:|nil|)))

(defun truthy? (x)
  (not (falsy? x)))

(defun dissect-seq-pattern (pats)
  (mvlet* ((pats (convert 'list pats))
           (all pats
            (match (last pats 2)
              ((list :|as| all)
               (values all (butlast pats 2)))
              (otherwise (values (string-gensym 'all) pats))))
           (rest pats
            (match (last pats 2)
              ((list '|clojure.core|:& rest)
               (values rest (butlast pats 2)))
              (otherwise (values nil pats)))))
    (values pats rest all (length pats))))

(defun seq->lambda-list (seq)
  (multiple-value-bind (pats rest all)
      (dissect-seq-pattern (convert 'list seq))
    (assert (not (symbol-package all)))
    (assert (every #'symbolp pats))
    (assert (symbolp rest))
    (append pats
            (and rest (list '|clojure.core|:&)))))

(defun fset-seq-pattern (pats)
  (multiple-value-bind (pats rest all len)
      (dissect-seq-pattern pats)
    `(trivia:guard1 (,all :type seq)
                    ;; Missing or excess elements are just bound to nil.
                    (typep ,all 'seq)
                    ,@(loop for pat in pats
                            for i from 0
                            collect `(lookup ,all ,i)
                            collect pat)
                    ,@(and rest
                           `((convert 'list (fset:subseq ,all ,len))
                             ,rest)))))

(defpattern fset-seq (&rest pats)
  (fset-seq-pattern pats))

(defpattern clojuresque-list (&rest pats)
  ;; NB This only works for lists with at least as many pats as are
  ;; present in PATS. They still get destructured, but as sequences
  ;; rather than lists, so less efficiently.
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

(defun safe-elt (seq i)
  (if (>= i (length seq)) nil
      (elt seq i)))

(defpattern clojuresque-sequence (&rest pats)
  (multiple-value-bind (pats rest all len)
      (dissect-seq-pattern pats)
    `(trivia:guard1 (,all :type sequence)
                    (typep ,all 'sequence)
                    ,@(loop for pat in pats
                            for i from 0
                            collect `(safe-elt ,all ,i)
                            collect pat)
                    ,@(and rest
                           `((drop ,len ,all)
                             ,rest)))))

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
  "Convert OBJ into a Trivia destructuring pattern.
Also return (as a second value) a list of all the symbols bound."
  (let ((syms (queue)))
    (labels ((map->alist (map)
               (collecting
                 (do-map (k v map)
                   (enq v syms)
                   (collect (cons k v)))))
             (obj->pattern (obj)
               (etypecase obj
                 (keyword obj)
                 (symbol
                  (enq obj syms)
                  obj)
                 (seq
                  (let ((pats (mapcar #'obj->pattern (convert 'list obj))))
                    ;; TODO vector, sequence
                    `(or (fset-seq ,@pats)
                         (clojuresque-list ,@pats)
                         ;; NB this matches lists with too few arguments.
                         (clojuresque-sequence ,@pats))))
                 (map
                  `(fset-map ,(map->alist obj))))))
      (values (obj->pattern obj)
              (qlist syms)))))

(defun fbind-keywords (keywords)
  (dolist (keyword (ensure-list keywords))
    (assert (keywordp keyword))
    (unless (fboundp keyword)
      (setf (symbol-function keyword)
            (lambda (map)
              (|clojure.core|:|get| map keyword))))))

;;; Macro helpers.

(defun parse-docs (body)
  (if (stringp (first body))
      (values (first body) (rest body))
      (values nil body)))

(defun car+cdr (list)
  (values (car list) (cdr list)))

(defun var (sym &optional env)
  (let ((exp (macroexpand-1 (assure symbol sym) env)))
    (when (or (eql exp sym)
              (not (symbolp exp))
              (not (meta-ref sym :|dynamic|)))
      (error "Not a var: ~a" sym))
    exp))

(defconstructor protocol
  (name symbol)
  (functions list))

(define-namespace protocol protocol)

(defun check-protocol (protocol-name fns)
  ;; TODO Are protocols supposed to be exhaustive?
  (let* ((protocol (symbol-protocol protocol-name))
         (protocol-fn-names (mapcar #'ensure-car (protocol-functions protocol)))
         (fn-names (mapcar #'ensure-car fns)))
    (assert (subsetp fn-names protocol-fn-names))))

(defun split-specs (specs)
  "Split the common Clojure syntax of a symbol (protocol, type) and a list of protocol/interface implementations."
  (if (emptyp specs) nil
      (runs specs :test (lambda (x y) (declare (ignore x))
                          (not (symbolp y))))))
