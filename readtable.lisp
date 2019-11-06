(in-package #:cloture)

(defunit eof)

(defun rec-read (stream)
  (read stream t nil t))

(defun read-meta (stream char)
  (declare (ignore char))
  (let ((meta (rec-read stream))
        (value (rec-read stream)))
    (merge-meta! value (ensure-meta meta))
    value))

(defun read-var (stream char arg)
  (declare (ignore char arg))
  (let ((sym (rec-read stream)))
    `(|clojure.core|:|var| ,sym)))

(defun read-conditional (stream char arg)
  (declare (ignore char arg))
  (let ((forms (rec-read stream)))
    (values
     (let* ((missing "missing")
            (cl (getf forms :|cl| missing)))
       (if (eql cl missing)
           (getf forms :|default|)
           cl)))))

(defun read-nothing (stream char arg)
  (declare (ignore char arg))
  (let ((*read-suppress* t))
    (rec-read stream)
    (values)))

(defconstructor regex
  (string string))

(defun read-regex (stream char arg)
  (declare (ignore arg))
  (unread-char char stream)
  (let ((string (assure string (rec-read stream))))
    (regex string)))

(defun read-quote (stream char)
  (declare (ignore char))
  `(|clojure.core|:|quote| ,(rec-read stream)))

(defreadtable cloture
  (:fuze :standard cloture.qq:quasiquote-mixin)
  ;; Clojure quote.
  (:macro-char #\' 'read-quote)
  ;; Supress.
  (:dispatch-macro-char #\# #\_ 'read-nothing)
  ;; Metadata.
  (:macro-char #\^ 'read-meta)
  ;; Reading regexes.
  (:dispatch-macro-char #\# #\" 'read-regex)
  ;; Reading maps and sets.
  (:syntax-from :standard #\) #\})
  ;; Reading seqs.
  (:syntax-from :standard #\) #\])
  ;; ~ instead of ,.
  (:syntax-from 'cloture.qq:quasiquote-mixin #\, #\~)
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

(defun resolve-slash-symbol (symbol)
  (match (symbol-name symbol)
    ((and symbol (type keyword))
     symbol)
    ((ppcre "(.+?)/(.+)" package-name symbol-name)
     (let* ((package
              (or (find-package package-name)
                  (error (clojure-reader-error "No such package as ~s" package-name)))))
       (find-external-symbol symbol-name package :error t)))
    (otherwise symbol)))

(defun fixup-symbols (tree)
  ;; TODO descend into maps, vectors, etc.
  (leaf-map (lambda (x)
              (if (symbolp x)
                  (resolve-slash-symbol x)
                  x))
            tree))

(defun subread-clojure (stream char arg)
  (declare (ignore char arg))
  (let* ((*package* (find-package :cloture.impl))
         (form (read-clojure stream :recursive t)))
    (fixup-symbols form)))

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
