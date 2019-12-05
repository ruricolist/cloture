(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun compile-and-eval (form)
  (funcall (compile nil (eval `(lambda () ,form)))))

(defun repl ()
  (with-clojure-printer ()
    (let ((*readtable* (find-readtable 'cloture))
          (*package* (find-package "user")))
      (catch 'quit
        (loop (format t "~&~a=> " (package-name *package*))
              (finish-output)
              (with-simple-restart (abort "Return to Clojure REPL")
                (let ((form (read)))
                  (format t "~s" (compile-and-eval form))
                  (finish-output))))))))

(defun-1 #_exit ()
  (throw 'quit (values)))

(defun-1 #_quit ()
  (#_exit))
