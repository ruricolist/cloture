(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun self-evaluating? (x)
  (typep x '(or keyword number string)))

(defun maybe-quote (x)
  (if (self-evaluating? x) x
      `(#_quote ,x)))

(defun pp-seq (stream seq)
  (pprint-logical-block (stream nil :prefix "[")
    (iterate (for x in-fset-seq seq)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write (maybe-quote x) :stream stream))
    (format stream " ]")))

(defun pp-map (stream map)
  (pprint-logical-block (stream nil :prefix "{")
    (iterate (for (x y) in-map map)
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
    (set-pprint-dispatch '(cons (eql #_quote)) #'pp-quote 0 table)
    (set-pprint-dispatch '(cons (eql quote)) nil 0 table)
    (set-pprint-dispatch '(cons (eql function)) nil 0 table)))

(defun call/clojure-printer (fn)
  (let ((*print-pretty* t)
        (*print-pprint-dispatch*
          *clojure-pprint-dispatch*))
    (funcall fn)))

(defmacro with-clojure-printer ((&key) &body body)
  (with-thunk (body)
    `(call/clojure-printer ,body)))
