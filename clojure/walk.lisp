(in-package #:cloture)
(in-readtable clojure-shortcut)

(deftype traversable ()
  '(or list
    seq map set
    map-entry
    sorted-map))

(defun-1 #_postwalk (f form)
  (fbind ((f (ifn-function f)))
    (labels ((postwalk (form)
               (typecase-of traversable form
                 (list (f (mapcar #'postwalk form)))
                 (set
                  (iterate (for elt in-set form)
                    (collecting-set (postwalk elt) into s)
                    (finally (return (f s)))))
                 (seq
                  (iterate (for elt in-seq form)
                    (collecting-seq (postwalk elt) into s)
                    (finally (return (f s)))))
                 (map
                  (iterate (for (k v) in-map form)
                    (collecting-map (postwalk k) (postwalk v) into m)
                    (finally (return (f m)))))
                 (sorted-map
                  (error (clojure-error "Cannot walk sorted map: ~a" form)))
                 (map-entry
                  (ematch form
                    ((map-entry k v)
                     (f (map-entry (postwalk k) (postwalk v))))))
                 (otherwise (f form)))))
      (postwalk form))))

(defun-1 #_postwalk-demo (form)
  (#_postwalk (lambda (form)
                (format t "~&Walked: ~a" form)
                form)
              form))

(defun-1 #_postwalk-replace (smap form)
  (#_postwalk (lambda (f)
                (#_lookup smap f f))
              form))
