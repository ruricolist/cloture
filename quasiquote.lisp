;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; pattern-matching friendly implementation of Quasiquote
;;; Copyright (c) 2002-2014 Fahree Reedaw <fare@tunes.org>
;;; See README

(defpackage :cloture.qq
  (:use :uiop :fare-utils :cl)
  (:import-from :fset :seq :convert :empty-map :empty-seq)
  (:shadowing-import-from :fset :map)
  (:shadow #:list #:list* #:cons #:append #:nconc #:quote)
  (:shadow #:kwote #:quotep #:n-vector #:make-vector)
  (:export #:quasiquote-expand #:quasiquote #:unquote #:unquote-splicing
	   #:enable-quasiquote #:*fq-readtable*
           #:enable-qq-pp #:*fq-pprint-dispatch*
           #:call-with-quasiquote-reader
           #:call-with-unquote-reader
           #:call-with-unquote-splicing-reader
           #:call-with-unquote-nsplicing-reader
           #:quasiquote-mixin))
(in-package :cloture.qq)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;;; uncomment some of the lines below to disable according simplifications:
  ;;(pushnew :quasiquote-strict-append *features*)
  ;;(pushnew :quasiquote-passes-literals *features*)
  ;;(pushnew :quasiquote-at-macro-expansion-time *features*)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Functions that actually build data structures.
;; Note that we want our own tokens for decompilation reasons,
;; but as functions they must evaluate the usual way.
(defsubst list (&rest r) r) ;; (apply #'cl:list r)
(defsubst list* (&rest r) (apply #'cl:list* r))
(defsubst cons (x y) (cl:cons x y))
(defsubst append (&rest r) (apply #'cl:append r))
(defsubst nconc (&rest r) (apply #'cl:nconc r))
;; These supporting functions don't have a standard name
(defsubst make-vector (l) (coerce l 'simple-vector))
(defsubst make-seq (l) (convert 'seq l))
(defsubst make-map (l)
  (let ((pairs (serapeum:batches l 2 :even t)))
    (reduce (lambda (map pair)
              (destructuring-bind (key value) pair
                (fset:with map key value)))
            pairs
            :initial-value (empty-map))))
(defun n-vector (n contents)
  (if (null n) (make-vector contents)
      (let ((a (make-array n :element-type t)))
        (when (and (null contents) (> n 0))
          (error "non-zero length vector with empty contents"))
        (loop for i below n with x
              do (unless (null contents) (setq x (pop contents)))
              do (setf (aref a i) x))
        (when contents
          (error "provided contents larger than declared vector length"))
        a)))

(defun n-seq (n contents)
  (declare (ignore n))
  (make-seq contents))

(defun n-map (n contents)
  (declare (ignore n))
  (make-map contents))

;;; These functions build the forms that build the data structures.
(make-single-arg-form quote kwote)
(make-single-arg-form quasiquote)
(make-single-arg-form unquote)
(make-single-arg-form unquote-splicing)
(make-single-arg-form unquote-nsplicing)
(defun k-list (&rest r) (cons 'list r))
(defun k-list-p (x) (and (consp x) (eq (car x) 'list)))
(defun k-list* (&rest r) (cons 'list* r))
(defun k-list*-p (x) (and (consp x) (eq (car x) 'list*)))
(defun k-cons (x y) (list 'cons x y))
(defun k-cons-p (x) (and (consp x) (eq (car x) 'cons)))
(defun k-append (&rest r) (cons 'append r))
(defun k-append-p (x) (and (consp x) (eq (car x) 'append)))
(defun k-nconc (&rest r) (cons 'nconc r))
(defun k-nconc-p (x) (and (consp x) (eq (car x) 'nconc)))

(defun k-literal (literal)
  #+quasiquote-passes-literals literal
  #-quasiquote-passes-literals (kwote literal))

;;; These macros expand into suitable forms
(defmacro quote (x) (list 'cl:quote x))
(defmacro quasiquote (x) (quasiquote-expand x))
(defmacro unquote (x)
  (declare (ignore x))
  (error "unquote only allowed within quasiquote"))
(defmacro unquote-splicing (x)
  (declare (ignore x))
  (error "unquote-splicing disallowed outside quasiquote"))
(defmacro unquote-nsplicing (x)
  (declare (ignore x))
  (error "unquote-nsplicing disallowed outside quasiquote"))

(defun quasiquote-form-p (x)
  (or (quotep x)
      (k-list-p x)
      (k-list*-p x)
      (k-cons-p x)
      (k-append-p x)
      (k-nconc-p x)
      (k-n-vector-p x)
      (k-n-seq-p x)
      (k-n-map-p x)))

(defun k-n-vector (n l)
  (cond
    ((null l)
     (k-literal (vector)))
    ((quotep l)
     (k-literal (n-vector n (single-arg l))))
    (n
     (list 'n-vector n l))
    (t
     (list 'make-vector l))))

(defun k-n-seq (n l)
  (cond
    ((null l)
     (k-literal (seq)))
    ((quotep l)
     (k-literal (n-seq n (single-arg l))))
    (n (list 'n-seq n l))
    (t (list 'make-seq l))))

(defun k-n-map (n l)
  (cond
    ((null l)
     (k-literal (fset:empty-map)))
    ((quotep l)
     (k-literal (n-map n (single-arg l))))
    (n (list 'n-map n l))
    (t (list 'make-map l))))

(defun k-n-vector-p (x) (and (consp x) (member (first x) '(make-vector n-vector))))
(defun k-n-seq-p (x)
  (and (consp x)
       (member (first x) '(make-seq n-seq))))

(defun k-n-map-p (x)
  (and (consp x)
       (member (first x) '(make-map n-map))))

(defun valid-k-n-vector-p (x)
  (or (and (length=n-p x 3) (eq (first x) 'n-vector)
           (typep (second x) `(or null (integer 0 ,array-rank-limit)))
           #+quasiquote-strict-append
           (quasiquote-form-p (third x)))
      (and (length=n-p x 2) (eq (first x) 'make-vector)
           #+quasiquote-strict-append
           (quasiquote-form-p (second x)))))

(defun valid-k-n-seq-p (x)
  (or (and (length=n-p x 3)
           (eq (first x) 'n-seq)
           (typep (second x) `(or null (integer 0 ,array-rank-limit)))
           #+quasiquote-strict-append
           (quasiquote-form-p (third x)))
      (and (length=n-p x 2) (eq (first x) 'make-seq)
           #+quasiquote-strict-append
           (quasiquote-form-p (second x)))))

(defun valid-k-n-map-p (x)
  (or (and (length=n-p x 3)
           (eq (first x) 'n-map)
           (typep (second x) `(or null (integer 0 ,array-rank-limit)))
           #+quasiquote-strict-append
           (quasiquote-form-p (third x)))
      (and (length=n-p x 2) (eq (first x) 'make-map)
           #+quasiquote-strict-append
           (quasiquote-form-p (second x)))))

(defun k-n-vector-n (x)
  (and (valid-k-n-vector-p x) (eq (first x) 'n-vector) (second x)))

(defun k-n-seq-n (x)
  (and (valid-k-n-seq-p x)
       (eq (first x) 'n-seq)
       (second x)))

(defun k-n-map-n (x)
  (and (valid-k-n-map-p x)
       (eq (first x) 'n-map)
       (second x)))

(defun k-n-vector-contents (x)
  (and (valid-k-n-vector-p x)
       (ecase (first x)
         ((make-vector) (second x))
         ((n-vector) (third x)))))

(defun k-n-seq-contents (x)
  (and (valid-k-n-seq-p x)
       (ecase (first x)
         ((make-seq) (second x))
         ((n-seq) (third x)))))

(defun k-n-map-contents (x)
  (and (valid-k-n-map-p x)
       (ecase (first x)
         ((make-map) (second x))
         ((n-map) (third x)))))

(defun properly-ended-list-p (x)
  (and (listp x) (null (cdr (last x)))))

(defparameter *quasiquote-level* 0
  "current depth of quasiquote nesting")

(defun unquote-xsplicing-p (x)
  (or (unquote-splicing-p x) (unquote-nsplicing-p x)))

(defun quasiquote-expand (x)
  (let ((*quasiquote-level* 0))
    (multiple-value-bind (top arg)
        (quasiquote-expand-0 x)
      (when (eq top 'unquote-splicing)
        (error ",@ after backquote in ~S" x))
      (when (eq top 'unquote-nsplicing)
        (error ",. after backquote in ~S" x))
      (quasiquote-expand-1 top arg))))

(defun quasiquote-expand-0 (x)
  "Given an expression x under a backquote, return two values:
1- a token identifying the context: nil quote :literal list list* append nconc
2- a form
When combining backquoted expressions, tokens are used for simplifications."
  (cond
    ((null x)
     (values nil nil))
    ((literalp x)
     (values #+quasiquote-passes-literals :literal #-quasiquote-passes-literals 'quote x))
    ((or (symbolp x) (quotep x))
     (values 'quote x))
    ((unquote-splicing-p x)
     (values 'unquote-splicing (single-arg x)))
    ((unquote-nsplicing-p x)
     (values 'unquote-nsplicing (single-arg x)))
    ((unquotep x)
     (values 'unquote (single-arg x)))
    ((quasiquotep x)
     (quasiquote-expand-0 (quasiquote-expand (single-arg x))))
    ((k-n-vector-p x)
     (values (car x) (cdr x)))
    ((k-n-seq-p x)
     (values (car x) (cdr x)))
    ((k-n-map-p x)
     (values (car x) (cdr x)))
    ((consp x)
     (multiple-value-bind (atop a) (quasiquote-expand-0 (car x))
       (multiple-value-bind (dtop d) (quasiquote-expand-0 (cdr x))
         (when (eq dtop 'unquote-splicing)
           (error ",@ after dot"))
         (when (eq dtop 'unquote-nsplicing)
           (error ",. after dot"))
         (cond
           ((eq atop 'unquote-splicing)
            (cond
              #-quasiquote-strict-append
              ((null dtop)
               (if (unquote-xsplicing-p a)
                   (values 'append (list a))
                   (expand-unquote a)))
              (t
               (values 'append
                       (cond ((eq dtop 'append)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d))))))))
           ((eq atop 'unquote-nsplicing)
            (cond
              #-quasiquote-strict-append
              ((null dtop)
               (if (unquote-nsplicing-p a)
                   (values 'nconc (list a))
                   (expand-unquote a)))
              (t
               (values 'nconc
                       (cond ((eq dtop 'nconc)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d))))))))
           ((null dtop)
            (if (member atop '(quote :literal nil))
                (values 'quote (list a))
                (values 'list (list (quasiquote-expand-1 atop a)))))
           ((member dtop '(quote :literal))
            (cond
              ((member atop '(quote :literal nil))
               (values 'quote (cons a d)))
              ;; This should be done more cautiously.
              ;; Can we detect the case "has no (recursive) quasiquote escapes"?
              ;; Or is 'quote already that?
              #|
              ((and (consp d) (null (cdr (last d))))
              (values 'list (list* (quasiquote-expand-1 atop a)
              (mapcar 'kwote d)))) |#
              (t
               (values 'list* (list (quasiquote-expand-1 atop a)
                                    (quasiquote-expand-1 dtop d))))))
           (t (let ((qa (quasiquote-expand-1 atop a)))
                (if (member dtop '(list list*))
                    (values dtop (cons qa d))
                    (values 'list*
                            (list qa (quasiquote-expand-1 dtop d))))))))))
    (t
     (error "unrecognized object in quasiquote"))))

(defun expand-unquote (x)
  (cond
    ((null x)
     (values nil nil))
    ((literalp x)
     (values #+quasiquote-passes-literals :literal #-quasiquote-passes-literals 'quote x))
    ((symbolp x)
     (values 'unquote x))
    ((not (consp x))
     (error "unrecognized object in unquote"))
    ((and (quotep x)
          (not (unquote-xsplicing-p (single-arg x))))
     (values 'quote (single-arg x)))
    ((member (car x) '(list list* append nconc))
     (values (car x) (cdr x)))
    ((eq (car x) 'cons)
     (values 'list* (cdr x)))
    (t (values 'unquote x))))

(defun quasiquote-expand-1 (top x)
  "Given a top token and an expression, give the quasiquoting
of the result of the top operation applied to the expression"
  (cond
    ((member top '(unquote :literal nil))
     x)
    ((eq top 'quote)
     (kwote x))
    ((member top '(cons list*))
     (cond
       #-quasiquote-strict-append
       ((length=n-p x 1) x)
       ((let ((last (last x)))
          (when (or (null last) (and (consp last) (quotep (car last))
                                     (properly-ended-list-p (single-arg (car last)))))
            (quasiquote-expand-1 'list (append (butlast x)
                                               (mapcar 'kwote (and last (single-arg (car last)))))))))
       ((length=n-p x 2)
        (apply 'k-cons x))
       ((unquote-xsplicing-p (car (last x)))
        (k-append
         (quasiquote-expand-1 'list (butlast x))
         (car (last x))))
       (t
        (apply 'k-list* x))))
    (t
     (cons (ecase top
             ((list cons append nconc
                    make-vector n-vector
                    make-seq n-seq
                    make-map n-map)
              top))
           x))))

;; Note: it would be a *very bad* idea to use quasiquote:quote
;; in the expansion of the macro-character #\'

(defun call-with-quasiquote-reader (thunk)
  (let ((*quasiquote-level* (1+ *quasiquote-level*)))
    (make-quasiquote (funcall thunk))))

(defun call-with-unquote-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote outside quasiquote"))
    (make-unquote (funcall thunk))))

(defun call-with-unquote-splicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-splicing outside quasiquote"))
    (make-unquote-splicing (funcall thunk))))

(defun call-with-unquote-nsplicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-nsplicing outside quasiquote"))
    (make-unquote-nsplicing (funcall thunk))))

(defun read-quasiquote (stream)
  (call-with-quasiquote-reader (lambda () (read stream t nil t))))

(defun read-unquote (stream)
  (call-with-unquote-reader (lambda () (read stream t nil t))))

(defun read-unquote-splicing (stream)
  (call-with-unquote-splicing-reader (lambda () (read stream t nil t))))

(defun read-unquote-nsplicing (stream)
  (call-with-unquote-nsplicing-reader (lambda () (read stream t nil t))))

(defun read-vector (stream n)
  ;; http://www.lisp.org/HyperSpec/Body/sec_2-4-8-3.html
  (if (= *quasiquote-level* 0)
      (n-vector n (read-delimited-list #\) stream t))
      (make-unquote
       (k-n-vector n (quasiquote-expand
                      (progn (unread-char #\( stream)
                             (read-preserving-whitespace stream t nil t)))))))

(defun read-seq (stream n)
  (if (= *quasiquote-level* 0)
      (n-seq n (read-delimited-list #\] stream t))
      (make-unquote
       (k-n-seq n (quasiquote-expand
                   (read-delimited-list #\] stream t))))))

(defun read-map (stream n)
  (if (= *quasiquote-level* 0)
      (n-map n (read-delimited-list #\} stream t))
      (make-unquote
       (k-n-map n (quasiquote-expand
                   (read-delimited-list #\} stream t))))))

(defun read-read-time-backquote (stream char)
  (declare (ignore char))
  (values (macroexpand-1 (read-quasiquote stream))))
(defun read-macroexpand-time-backquote (stream char)
  (declare (ignore char))
  (read-quasiquote stream))
(defun read-backquote (stream char)
  #-quasiquote-at-macro-expansion-time (read-read-time-backquote stream char)
  #+quasiquote-at-macro-expansion-time (read-macroexpand-time-backquote stream char))
(defun backquote-reader (expansion-time)
  (ecase expansion-time
    ((read) #'read-read-time-backquote)
    ((macroexpand) #'read-macroexpand-time-backquote)
    ((nil) #'read-backquote)))
(defun read-comma (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t nil t)
    ((#\@)
     (read-char stream t nil t)
     (read-unquote-splicing stream))
    ((#\.)
     (read-char stream t nil t)
     (read-unquote-nsplicing stream))
    (otherwise (read-unquote stream))))
(defun read-hash-paren (stream subchar arg)
  (declare (ignore subchar))
  (read-vector stream arg))

(defvar *hash-dot-reader* (get-dispatch-macro-character #\# #\.))

(defun read-hash-dot (stream subchar arg)
  (let ((*quasiquote-level* 0))
    (funcall *hash-dot-reader* stream subchar arg)))

);eval-when

(eval-now
  (named-readtables:defreadtable quasiquote-mixin
    (:macro-char #\` 'read-read-time-backquote)
    (:macro-char #\, 'read-comma)
    (:macro-char #\# :dispatch)
    (:dispatch-macro-char #\# #\( 'read-hash-paren)
    (:dispatch-macro-char #\# #\. 'read-hash-dot)
    (:macro-char #\[ 'read-seq)
    (:macro-char #\{ 'read-map)
    (:syntax-from :standard #\) #\])))
