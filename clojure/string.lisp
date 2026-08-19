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
  ((coll) (string-join coll ""))
  ((sep coll) (string-join coll sep)))

(defun replace-aux (s match replacement)
  (multiple-value-ematch (values match replacement)
    (((type character) (type character))
     (substitute match replacement s))
    (((type string) (type string))
     (string-replace-all match s replacement))
    (((type raw-regex) _)
     (replace-aux s
                  (ppcre:create-scanner
                   (raw-regex-string match))
                  replacement))
    (((type compiled-regex) _)
     (replace-aux s
                  (compiled-regex-function match)
                  replacement))
    (((type function) (type string))
     (replace-aux s match (parse-replacement-string replacement)))
    (((type function) (type replacement-string))
     (ppcre:regex-replace-all match s (replacement-string-list replacement)))))

;;; TODO Compiler macro for the regex and the replacement string.
(defun-1 |clojure.string|:|replace| (s match replacement)
  (replace-aux s match replacement))

(defun-1 |clojure.string|:|upper-case| (s)
  (check-type s string)
  (string-upcase s))

(defun-1 |clojure.string|:|lower-case| (s)
  (check-type s string)
  (string-downcase s))

(defconst string-whitespace
  '(#\Space #\Tab #\Newline #\Return #\Page #\Backspace))

(defun-1 |clojure.string|:|trim| (s)
  (check-type s string)
  (string-trim string-whitespace s))

(defun-1 |clojure.string|:|triml| (s)
  (check-type s string)
  (string-left-trim string-whitespace s))

(defun-1 |clojure.string|:|trimr| (s)
  (check-type s string)
  (string-right-trim string-whitespace s))

(defun-1 |clojure.string|:|trim-newline| (s)
  (check-type s string)
  (string-right-trim '(#\Newline #\Return) s))

(defun-1 |clojure.string|:|blank?| (s)
  (? (or (not (stringp s))
         (every #'whitespacep s))))

(defun-1 |clojure.string|:|capitalize| (s)
  (check-type s string)
  (if (< (length s) 2)
      (string-upcase s)
      (string+ (string-upcase (subseq s 0 1))
               (string-downcase (subseq s 1)))))

(defun-1 |clojure.string|:|includes?| (s substring)
  (check-type s string)
  (? (search (string substring) s)))

(defn |clojure.string|:|index-of|
  ((s value) (or (search (string value) s) #_nil))
  ((s value from) (or (search (string value) s :start2 from) #_nil)))

(defn |clojure.string|:|last-index-of|
  ((s value) (or (search (string value) s :from-end t) #_nil))
  ((s value from) (or (search (string value) s :from-end t :end2 (min (length s) (+ from (length (string value))))) #_nil)))

(defn |clojure.string|:|split|
  ((s re) (convert 'seq (ppcre:split (compile-re re) s)))
  ((s re limit)
   (convert 'seq (ppcre:split (compile-re re) s
                              :limit (if (plusp limit) limit nil)))))

(defun-1 |clojure.string|:|split-lines| (s)
  (check-type s string)
  (convert 'seq (ppcre:split "\\r?\\n" s)))

(defun-1 |clojure.string|:|escape| (s cmap)
  (check-type s string)
  (with-output-to-string (out)
    (loop for c across s
          for replacement = (#_get cmap c #_nil)
          do (if (eql replacement #_nil)
                 (write-char c out)
                 (princ replacement out)))))
