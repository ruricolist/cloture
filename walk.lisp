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
                (otherwise tree)))
            tree))
