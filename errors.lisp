(in-package :cloture)
(in-readtable clojure-shortcut)

(defcondition clojure-condition () ())

(defgeneric #_.getMessage (condition)
  (:method ((c condition))
    (princ-to-string c)))

(defcondition clojure-error (error clojure-condition)
  ((message :initarg :message)
   (cause :initarg :cause :reader #_.getCause))
  (:documentation "Sub-root of all Clojure conditions.")
  (:default-initargs :cause #_nil)
  (:report (lambda (c s)
             (with-slots (message) c
               (format s "~a" message)))))

(defmacro define-simple-error-constructor (name)
  (let* ((ctor-name (string+ name "."))
         (ctor (find-external-symbol ctor-name (symbol-package name) :error t)))
    `(defsubst ,ctor (msg)
       (make-condition ',name :message msg))))

(defmacro defcondition* (name supers &body body)
  `(progn
     (defcondition ,name ,supers
       ,@(if body body (list nil)))
     (define-symbol-macro ,name (find-class ',name))))

(defcondition* #_Throwable (clojure-error))
(define-simple-error-constructor #_Throwable)

(defcondition* #_Exception (#_Throwable) ())
(define-simple-error-constructor #_Exception)

(defcondition* #_RuntimeException (#_Exception) ())
(define-simple-error-constructor #_RuntimeException)

(defcondition* #_IllegalArgumentException (#_RuntimeException) ())
(define-simple-error-constructor #_IllegalArgumentException)

(defcondition* #_IllegalStateException (#_RuntimeException) ())
(define-simple-error-constructor #_IllegalStateException)

(defcondition* #_ArityException (#_IllegalArgumentException)
  ((actual :initarg :actual)
   (name :initarg :name))
  (:report (lambda (c s)
             (with-slots (name actual) c
               (format s "~a got ~a arg~:p, which is the wrong arity."
                       (or name "Anonymous function") actual)))))
(defun #_ArityException. (actual name)
  (make-condition '#_ArityException
                  :actual actual
                  :name name))

(defcondition* #_Error (#_Exception) ())
(define-simple-error-constructor #_Error)

(defcondition* #_AssertionError (#_Exception) ())
(define-simple-error-constructor #_AssertionError)

(defcondition* #_IllegalAccessError (#_Error) ()) ;Skipping some parents.
(define-simple-error-constructor #_IllegalAccessError)

(defcondition already-persistent (#_IllegalAccessError)
  ((transient :initarg :transient))
  (:report (lambda (c s)
             (with-slots (transient) c
               (format s "Transient ~a has already been persisted."
                       transient)))))

(defcondition not-yet-implemented (#_Throwable)
  ((what :initarg :what))
  (:report (lambda (c s)
             (with-slots (what) c
               (format s "Not yet implemented: ~a" what)))))

(defcondition simple-clojure-error (clojure-condition simple-error) ())
(defcondition clojure-program-error (program-error clojure-error) ())
(defcondition simple-clojure-program-error (clojure-program-error simple-condition) ())
(defcondition clojure-reader-error (clojure-error reader-error) ())
(defcondition simple-clojure-reader-error (simple-clojure-error reader-error) ())
(defcondition clojure-package-error (clojure-error package-error) ())
(defcondition clojure-syntax-error (clojure-error) ())
(defcondition simple-clojure-syntax-error (simple-error clojure-syntax-error) ())

(defcondition wrong-number-arguments (clojure-program-error)
  ((arguments :initarg :arguments)))

(defcondition too-many-arguments (wrong-number-arguments)
  ((max :initarg :max :type (integer 0 *)))
  (:report (lambda (c s)
             (with-slots (arguments max) c
               (format s "Too many arguments (max ~a):~%~s" max arguments)))))

(defcondition too-few-arguments (wrong-number-arguments)
  ((min :initarg :max :type (integer 0 *)))
  (:report (lambda (c s)
             (with-slots (arguments max) c
               (format s "Too many arguments (max ~a):~%~s" max arguments)))))

(defun clojure-error (control &rest args)
  (make-condition 'simple-clojure-error
                  :format-control control
                  :format-arguments args))

(defun clojure-syntax-error (control &rest args)
  (make-condition 'simple-clojure-syntax-error
                  :format-control control
                  :format-arguments args))

(defun clojure-program-error (control &rest args)
  (make-condition 'simple-clojure-program-error
                  :format-control control
                  :format-arguments args))

(defun clojure-reader-error (control &rest args)
  (make-condition 'simple-clojure-reader-error
                  :format-control control
                  :format-arguments args))

(defun too-many-arguments (max-arity args)
  (error 'too-many-arguments
         :max max-arity
         :arguments args))

(defun too-few-arguments (max-arity args)
  (error 'too-few-arguments
         :max max-arity
         :arguments args))

(defcondition does-not-extend (clojure-error)
  ((protocol :initarg :protocol)
   (object :initarg :object))
  (:report (lambda (c s)
             (with-slots (protocol object) c
               (format s "Class of ~a does not extend protocol ~a"
                       object protocol)))))

(defcondition no-such-method (clojure-error)
  ((multi :initarg :multi)
   (value :initarg :value))
  (:report (lambda (c s)
             (with-slots (multi value) c
               (format s "No method for ~a in multimethod ~a" value multi)))))
