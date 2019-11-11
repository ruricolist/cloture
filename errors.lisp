(in-package :cloture)

(defcondition clojure-condition () ())
(defcondition clojure-error (clojure-condition error) ())
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
