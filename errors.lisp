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
