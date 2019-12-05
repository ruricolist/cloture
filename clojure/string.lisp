(in-package #:cloture)
(in-readtable clojure-shortcut)

(esrap:defrule replacement-string-special
    (and #\\ character)
  (:lambda (s)
    (esrap:text (second s))))
(esrap:defrule replacement-string-replacement
    (and #\$ (or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (:lambda (res)
    (1- (parse-integer (second res)))))
(esrap:defrule replacement-string-normal
    (+ (not (or #\\ #\$))))
(esrap:defrule replacement-string
    (+ (or replacement-string-special
         replacement-string-replacement
         replacement-string-normal))
  (:lambda (res)
    (loop for form in res
          collect (etypecase form
                    (list (coerce form 'string))
                    (string form)
                    (number form)))))

(defconstructor replacement-string
  (list list))

(defun parse-replacement-string (s)
  (replacement-string
   (esrap:parse 'replacement-string s)))

(defun-1 #_re-quote-replacement (s)
  (let ((parse (parse-replacement-string s)))
    (with-output-to-string (out)
      (dolist (elt (replacement-string-list parse))
        (etypecase elt
          (string (write-string elt out))
          (number (format out "\\$~a" (1+ elt))))))))

(defun-1 #_starts-with? (string prefix)
  (? (string^= prefix string)))

(defun-1 #_ends-with? (string suffix)
  (? (string$= suffix string)))

(defn #_join
  ((coll) (string-join coll))
  ((sep coll) (string-join coll sep)))

(defun replace-aux (s match replacement)
  (multiple-value-ematch (values match replacement)
    (((type character) (type character))
     (substitute match replacement s))
    (((type string) (type string))
     (string-replace-all match s replacement))
    (((type regex) _)
     (replace-aux s
                  (ppcre:create-scanner
                   (regex-string match))
                  replacement))
    (((type function) (type string))
     (replace-aux s match (parse-replacement-string replacement)))
    (((type function) (type replacement-string))
     (ppcre:regex-replace-all match s (replacement-string-list replacement)))))

;;; TODO Compiler macro for the regex and the replacement string.
(defun-1 |clojure.string|:|replace| (s match replacement)
  (replace-aux s match replacement))
