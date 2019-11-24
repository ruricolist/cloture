(in-package #:cloture)

(defunit eof)

(defun subread (stream)
  (read stream t nil t))

(defun read-meta (stream char)
  (declare (ignore char))
  (let* ((meta (clojurize (subread stream)))
         (value (clojurize (subread stream))))
    (merge-meta! value (ensure-meta meta))
    value))

(defun read-var (stream char arg)
  (declare (ignore char arg))
  (let ((sym (subread stream)))
    `(|clojure.core|:|var| ,sym)))

(defun read-conditional (stream char arg)
  (declare (ignore char arg))
  (let ((forms (subread stream)))
    (values
     (let* ((missing "missing")
            (cl (getf forms :|cl| missing)))
       (if (eql cl missing)
           (getf forms :|default|)
           cl)))))

(defun read-nothing (stream char arg)
  (declare (ignore char arg))
  (let ((*read-suppress* t))
    (subread stream)
    (values)))

(defconstructor regex
  (string string))

(defun read-regex (stream char arg)
  (declare (ignore arg))
  (unread-char char stream)
  (let ((string (assure string (subread stream))))
    (regex string)))

(defun read-quote (stream char)
  (declare (ignore char))
  `(|clojure.core|:|quote| ,(subread stream)))

(defvar *anon*)

(defconst max-anon-arg 20)

(defunion %arg
  %&
  (%n
   (pos (integer 0 #.max-anon-arg))))

(defconst anon-arg-syms
  (coerce (loop for i from 1 to max-anon-arg
                collect (intern (string+ "%" i)))
          'vector))

(defun %arg-sym (arg)
  (ematch arg
    ((eql %&) '%&)
    ((%n n) (aref anon-arg-syms (1- n)))))

(defclass function-literal ()
  ((args :initform nil :accessor function-literal-args)))

(defmethod function-literal-lambda ((anon function-literal) forms)
  (with-slots (args) anon
    (multiple-value-bind (max-offset variadic?)
        (mvfold (lambda (max variadic? arg)
                  (ematch arg
                    ((eql %&) (values max t))
                    ((%n n) (values (max max n) variadic?))))
                args
                0 nil)
      (let ((pos-args (coerce (take max-offset anon-arg-syms) 'list))
            (rest (and variadic? '(&rest %&))))
        ;; NB This must return a lambda so it can be used in function position.
        `(lambda (,@pos-args ,@rest)
           ,forms)))))

(defun read-anon (stream char arg)
  (declare (ignore char arg))
  (if (boundp '*anon*)
      (error (|clojure.core|:|IllegalStateException.|
                            "Anonymous function literals cannot be nested."))
      (let* ((anon (make 'function-literal))
             (*anon* anon)
             (forms
               (let ((*readtable* (find-readtable 'function-literal)))
                 (read-delimited-list #\) stream t))))
        (function-literal-lambda anon forms))))

(defun read-%arg (stream char)
  (declare (ignore char))
  (assert (boundp '*anon*))
  (let* ((next (read-char stream nil nil t))
         (arg
           (cond
             ((eql next #\&) %&)
             ((digit-char-p next)
              (let* ((chars
                       (cons next
                             (loop while (digit-char-p (peek-char nil stream))
                                   collect (read-char stream nil nil t))))
                     (chars (coerce 'string chars))
                     (n (parse-integer chars)))
                (%n n)))
             (t
              (unread-char next stream)
              (%n 1)))))
    (push arg (function-literal-args *anon*))
    (%arg-sym arg)))

(defun read-deref (stream char)
  (declare (ignore char))
  `(|clojure.core|:|deref| ,(subread stream)))

(defalias read-eval
  (get-dispatch-macro-character #\# #\.))

(defun read-string-with-escapes (stream char)
  (unread-char char stream)
  (let ((interpol:*outer-delimiters* '(#\"))
        (interpol:*inner-delimiters* nil)
        (interpol:*interpolate-format-directives* nil)
        (interpol::*regex-delimiters* nil))
    (interpol:interpol-reader stream nil nil)))

(defreadtable cloture
  (:fuze :standard cloture.qq:quasiquote-mixin)
  (:case :preserve)
  ;; Clojure quote.
  (:macro-char #\' 'read-quote)
  ;; Strings with escapes.
  (:macro-char #\" 'read-string-with-escapes)
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
  ;; Vars.
  (:dispatch-macro-char #\# #\' 'read-var)
  ;; Deref.
  (:macro-char #\@ #'read-deref)
  ;; Anonymous function.
  (:dispatch-macro-char #\# #\( 'read-anon)
  ;; Read eval.
  (:dispatch-macro-char #\# #\= 'read-eval))

(defreadtable function-literal
  (:merge cloture)
  (:case :preserve)
  (:macro-char #\% 'read-%arg t))

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
  (if (keywordp symbol) symbol
      (match (symbol-name symbol)
        ((ppcre "(.+?)/(.+)" package-name symbol-name)
         (let* ((package
                  (or (find-package package-name)
                    (error (clojure-reader-error "No such package as ~s" package-name)))))
           (find-external-symbol symbol-name package :error t)))
        (otherwise symbol))))

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
                                      end
                                      (package *package*))
  (let ((*package* (find-package package)))
    (with-input-from-string (in string :start start :end end)
      (read-clojure in :eof-error-p eof-error-p
                       :eof-value eof-value))))

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
