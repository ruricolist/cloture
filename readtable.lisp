(in-package #:cloture)

(defunit eof)

(defun rec-read (stream)
  (read stream t nil t))

(defun read-vector (stream char)
  (declare (ignore char))
  (convert 'seq (read-delimited-list #\] stream t)))

(defun read-map (stream char)
  (declare (ignore char))
  (let* ((data (read-delimited-list #\} stream t))
         (pairs (batches data 2 :even t)))
    (reduce (lambda (map pair)
              (destructuring-bind (key value) pair
                (with map key value)))
            pairs
            :initial-value (empty-map))))

(defun read-meta (stream char)
  (declare (ignore char))
  (let ((meta (rec-read stream))
        (value (rec-read stream)))
    (merge-meta! value (ensure-meta meta))
    value))

(defun read-conditional (stream char arg)
  (declare (ignore char arg))
  (let ((forms (rec-read stream)))
    (values (getf forms :|cl|))))

(defun read-var (stream char arg)
  (declare (ignore char arg))
  (let ((sym (rec-read stream)))
    `(|clojure.core|:|var| ,sym)))

(defreadtable cloture
  (:fuze :standard :fare-quasiquote-mixin)
  ;; Metadata.
  (:macro-char #\^ 'read-meta)
  ;; Reading vectors.
  (:macro-char #\[ 'read-vector)
  (:syntax-from :standard #\) #\])
  ;; Reading maps.
  (:macro-char #\{ 'read-map)
  (:syntax-from :standard #\) #\})
  ;; ~ instead of ,.
  (:syntax-from :fare-quasiquote-mixin #\, #\~)
  ;; , is whitespace.
  (:syntax-from :standard #\Space #\,)
  ;; Reader conditionals.
  (:dispatch-macro-char #\# #\? 'read-conditional)
  ;; Dereference vars.
  (:dispatch-macro-char #\# #\' 'read-var)
  (:case :preserve))

(defreadtable clojure-shortcut
  (:merge :standard)
  (:dispatch-macro-char #\# #\_ 'subread-clojure))

(defun call/clojure-reader (fn)
  (let ((*readtable* (find-readtable 'cloture))
        (*package* (find-package "user"))
        (*read-default-float-format* 'double-float))
    (funcall fn)))

(defmacro with-clojure-reader ((&key) &body body)
  (with-thunk (body)
    `(call/clojure-reader ,body)))

(defun read-clojure (stream
                     &key (eof-error-p t)
                          eof-value
                          recursive)
  (with-clojure-reader ()
    (read stream eof-error-p eof-value recursive)))

(defun subread-clojure (stream char arg)
  (declare (ignore char arg))
  (let ((*package* (find-package "user")))
    (read-clojure stream :recursive t)))

(defun read-clojure-from-string (string
                                 &key (eof-error-p t)
                                      eof-value
                                      (start 0)
                                      end)
  (with-input-from-string (in string :start start :end end)
    (read-clojure in :eof-error-p eof-error-p
                     :eof-value eof-value)))

(defun slurp-clojure-stream (stream)
  (loop for form
          = (read-clojure stream
                          :eof-error-p nil
                          :eof-value eof)
        until (eql form eof)
        collect form))

(defun slurp-clojure-file (file)
  (with-input-from-file (stream file)
    (slurp-clojure-stream stream)))

(defun load-clojure (file &rest args)
  (with-clojure-reader ()
    (apply #'load file args)))

(defun compile-clojure (file &rest args)
  (with-clojure-reader ()
    (apply #'compile-file file args)))

(defun compile-load-clojure (file)
  (load (compile-clojure file)))
