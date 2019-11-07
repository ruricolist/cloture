(in-package #:cloture)
(in-readtable clojure-shortcut)

(defun-1 #_apply-template (argv expr values)
  (let ((argv (convert 'list (assure seq argv)))
        (values (convert 'list values)))
    `(symbol-macrolet ,(mapcar #'list argv values)
       ,expr)))

(define-clojure-macro #_do-template (argv expr &body values)
  `(#_do
    ,@(loop for value in values
            collect (#_apply-template argv expr value))))
