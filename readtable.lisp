(in-package #:cloture)

(defunit eof)

(defconst %splicing-conditional '%splicing-conditional)

(defmacro #.%splicing-conditional (&rest forms)
  `(progn ,@forms))

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
  (let* ((splicing?
           (and (eql #\@ (peek-char nil stream))
                (read-char stream)
                t)))
    (flet ((maybe-splice (form)
             (if splicing?
                 (ematch form
                   ((list* '[] subforms)
                    `(,%splicing-conditional ,@subforms)))
                 form)))
      (let* ((forms (subread stream))
             (missing "missing")
             (cl (getf forms :|cl| missing)))
        (if (eql cl missing)
            (let ((default (getf forms :|default| missing)))
              (if (eql default missing)
                  (values)
                  (maybe-splice default)))
            (maybe-splice cl))))))

(defun read-nothing (stream char arg)
  (declare (ignore char arg))
  (let ((*read-suppress* t))
    (subread stream)
    (values)))

(defunion regex
  (raw-regex (string string))
  (compiled-regex (string string) (function function)))

(defmethod fset:compare ((r1 <regex>) (r2 <regex>))
  (fset:compare (regex-string r1) (regex-string r2)))

(fset:define-cross-type-compare-methods <regex>)

(defun regex-string (r)
  (match-of regex r
    ((raw-regex s) s)
    ((compiled-regex s _) s)))

(defun read-regex (stream char arg)
  (declare (ignore arg))
  (unread-char char stream)
  (let ((string (assure string
                  ;; Don't interpret backslash escapes.
                  (let ((cloture.interpol:*regex-delimiters* '(#\")))
                    (cloture.interpol:interpol-reader stream nil nil)))))
    (raw-regex string)))

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

(defun read-string-with-escapes (stream &optional (char #\"))
  (unread-char char stream)
  (let ((cloture.interpol:*regex-delimiters* nil))
    (cloture.interpol:interpol-reader stream nil nil)))

(defun read-delimited-string (stream)
  (let ((stream
          (make-concatenated-stream
           (make-string-input-stream "\"")
           stream)))
    (read stream)))

(def char-names
  '(("newline" . #\Newline)
    ("space" . #\Space)
    ("tab" . #\Tab)
    ("formfeed" . #\Formfeed)
    ("backspace" . #\Backspace)
    ("return" . #\Return)))

(defun read-clojure-char (stream char)
  (let ((next-char (peek-char nil stream nil nil t)))
    (cond ((eql char next-char)
           (read-char stream nil nil t)
           #\\)
          ((alpha-char-p next-char)
           (let* ((name
                    (loop for c = (peek-char nil stream nil nil t)
                          while (and c (alphanumericp c))
                          collect (read-char stream nil nil t)))
                  (name (coerce name 'string)))
             (cond ((= (length name) 1)
                    (character name))
                   ((string^= "u" name)
                    (code-char (parse-integer name :start 1 :radix 16)))
                   (t
                    (or (assocdr name char-names :test #'equal)
                        (error "Invalid char: ~s" name))))))
          (t
           (read-char stream nil nil t)))))

(defun subread-with-prefix (prefix stream)
  (let ((stream (make-concatenated-stream
                 (make-string-input-stream prefix)
                 stream))
        (*readtable* (find-readtable :standard)))
    (subread stream)))

(defun read-clojure-number (stream char)
  (declare (ignore char))
  (let ((next (read-char stream nil nil t)))
    (cond ((whitespacep next) 0)
          ((eql next #\x)
           (subread-with-prefix "#x" stream))
          ((digit-char-p next 8)
           (subread-with-prefix (fmt "#o~a" next) stream))
          (t (unread-char next stream)
             0))))

(defreadtable cloture
  (:fuze :standard cloture.qq:quasiquote-mixin)
  (:case :preserve)
  ;; Clojure quote.
  (:macro-char #\' 'read-quote)
  ;; Clojure characters.
  (:macro-char #\\ 'read-clojure-char)
  ;; Clojure numbers.
  (:macro-char #\0 'read-clojure-number t)
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

(defun subread-clojure (stream char arg)
  (declare (ignore char arg))
  (let* ((*package* (find-package :cloture.impl))
         (form (read-clojure stream :recursive t)))
    (if (and (symbolp form)
             (eql (symbol-package form)
                  (find-package :cloture.impl)))
        (error "Read non-inherited symbol ~a in implementation package." form)
        form)))

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

(defun slurp-clojure-file (file &key (package *package*))
  (let ((*package* (find-package package)))
    (with-input-from-file (stream file)
      (slurp-clojure-stream stream))))

(defun load-clojure (file &rest args)
  (with-clojure-reader ()
    (apply #'load file args)))

(defun compile-clojure (file &rest args)
  (with-clojure-reader ()
    (apply #'compile-file file args)))

(defun compile-load-clojure (file)
  (load (compile-clojure file)))
