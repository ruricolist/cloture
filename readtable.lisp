(in-package #:cloture)

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
  (let ((meta (read stream nil nil t))
        (value (read stream nil nil t)))
    (merge-meta value (ensure-meta meta))))

(defun read-conditional (stream char arg)
  (declare (ignore char arg))
  (let ((forms (read stream)))
    (values (getf forms :cl))))

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
  (:case :invert))
