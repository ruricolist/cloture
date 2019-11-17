(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun self-evaluating? (x)
  (typep x '(or keyword number)))

(defun maybe-quote (x)
  (if (self-evaluating? x) x
      `(#_quote ,x)))

(defun pp-seq (stream seq)
  (pprint-logical-block (stream nil :prefix "[")
    (do-seq (x seq)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write (maybe-quote x) :stream stream))
    (format stream " ]")))

(defun pp-map (stream map)
  (pprint-logical-block (stream nil :prefix "{")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; TODO quote if necessary
      (write (maybe-quote x) :stream stream)
      (write-char #\Space stream)
      (write (maybe-quote y) :stream stream))
    (format stream "}")))

(defun pp-quote (stream list)
  (write-char #\' stream)
  (write (only-elt (rest list)) :stream stream))

(defparameter *clojure-pprint-dispatch*
  (lret ((table (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'fset:seq #'pp-seq 0 table)
    (set-pprint-dispatch 'fset:map #'pp-map 0 table)
    (set-pprint-dispatch '(cons (eql #_quote))
                         #'pp-quote 0 table)))

(defun repl ()
  (let ((*print-pretty* t)
        (*print-pprint-dispatch*
          *clojure-pprint-dispatch*)
        (*readtable* (find-readtable 'cloture))
        (*package* (find-package "user")))
    (catch 'quit
      (loop (format t "~&~a=> " (package-name *package*))
            (let ((form (read)))
              (format t "~&~s~%" (eval form))
              (finish-output))))))

(defun-1 #_exit ()
  (throw 'quit (values)))

(defun-1 #_quit ()
  (#_exit))
