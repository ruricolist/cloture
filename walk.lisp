(in-package #:cloture)
(in-readtable clojure-shortcut)

(defconst special-forms
  '#_(
      &
      catch
      def
      def-
      do
      finally
      fn
      if
      let
      letfn
      loop
      monitor-enter
      monitor-exit
      new
      quote
      recur
      reify
      set!
      throw
      try
      var
      |.|))

(defun special-form? (form)
  (and (consp form)
       (symbolp (car form))
       (member (car form) special-forms)))

(defun splice-splicing-conditional (obj)
  (flet ((splice? (elt)
           (and (listp elt)
                (eql (car elt) %splicing-conditional))))
    (if (listp obj)
        (if (member-if #'splice? obj)
            (mappend (lambda (elt)
                       (if (splice? elt)
                           (cdr elt)
                           (list elt)))
                     obj)
            obj)
        obj)))

(defun declojurize (tree)
  "Replace literal objects (outside quasiquotes) with constructors.
Code is declojurized (for compilation by Lisp) when it is returned
from Clojure macros."
  (map-tree (named-lambda rec (tree)
              (match tree
                ((type seq)
                 `([]
                   ,@(mapcar (op (map-tree #'rec _))
                             (convert 'list tree))))
                ((type set)
                 `(|#{}|
                   ,@(mapcar (op (map-tree #'rec _))
                             (convert 'list tree))))
                ((type map)
                 `(|{}|
                   ,@(mapcar (op (map-tree #'rec _))
                             (map->list tree))))
                (otherwise tree)))
            tree))

(defun clojurize (tree)
  "Replace calls to constructors with literal objects.
Also convert the symbols for true, false, and nil to unit types.

Code is \"clojurized\" before being passed to Clojure macros, and when
returned by Clojure's quote."
  (map-tree (named-lambda rec (tree)
              (let ((tree (splice-splicing-conditional tree)))
                (match tree
                  ((list* '[] elts)
                   (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                     (convert 'seq elts)))
                  ((list* '|#{}| elts)
                   (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                     (convert 'set elts)))
                  ((list* '{} elts)
                   (let ((elts (mapcar (op (map-tree #'rec _)) elts)))
                     (list->map elts)))
                  ('|clojure.core|:|true|  |clojure.core|:|true|)
                  ('|clojure.core|:|false| |clojure.core|:|false|)
                  ('|clojure.core|:|nil|   |clojure.core|:|nil|)
                  ((list* _ (and _ (not (type list))))
                   (error "Improper list in Clojure tree."))
                  (otherwise tree))))
            tree))

(defun clojure-macroexpand-1 (form &optional env)
  "Like `macroexpand-1', but bottoms out if it hits a Clojure special form or a call that is not in Clojure."
  (if (special-form? form)
      (values form nil)
      (multiple-value-bind (exp exp?)
          (macroexpand-1 form env)
        (if (not exp?) form
            (match exp
              ((list* (and sym (type symbol)) _)
               (if (clojure-symbol? sym)
                   (values exp t)
                   (values form nil)))
              ((and sym (type symbol))
               (if (clojure-symbol? sym)
                   (values exp t)
                   (values form nil)))
              (otherwise (values exp exp?)))))))

(defun clojure-macroexpand (form &optional env)
  (nlet lp ((form form)
            (expanded? nil))
    (multiple-value-bind (exp exp?)
        (clojure-macroexpand-1 form env)
      (if exp?
          (lp exp t)
          (values form expanded?)))))
