;;; Copyright (c) 2003-2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defpackage :cloture.interpol
  (:use :cl :cl-unicode :cl-ppcre)
  (:import-from :named-readtables
    :defreadtable)
  (:export
   :*list-delimiter*
   :*outer-delimiters*
   :*inner-delimiters*
   :*optional-delimiters-p*
   :*interpolate-format-directives*
   :interpol-reader))

(in-package :cloture.interpol)

(defvar *list-delimiter* #\Space
  "What is inserted between the elements of a list which is
interpolated by #\@.")

(defvar *inner-delimiters* '((#\( . #\))
                             (#\{ . #\})
                             (#\< . #\>)
                             (#\[ . #\]))
  "Legal delimiters for interpolation with #\$ and #\@.")

(defvar *outer-delimiters* '((#\( . #\))
                             (#\{ . #\})
                             (#\< . #\>)
                             (#\[ . #\])
                             #\/ #\| #\" #\' #\#)
  "Legal outer delimiters for CL-INTERPOL strings.")

(defvar *regex-delimiters* '(#\/)
  "Outer delimiters which automatically enable regex mode.")

(defvar *unicode-aliases*
  (make-hash-table :test #'equalp)
  "A hash table which maps Unicode aliases to their real names.")

(defvar *optional-delimiters-p* nil
  "Whether text following $ or @ should interpolate even without a
following delimiter.  Lexical variables are handled correctly,
but the rules are somewhat complex -- see the docs for details.")

(defvar *interpolate-format-directives* nil
  "Whether to allow ~X(...) as format control directives in interpolated strings.")

(defmacro defvar-unbound (variable-name documentation)
  "Like DEFVAR, but the variable will be unbound rather than getting
an initial value.  This is useful for variables which should have no
global value but might have a dynamically bound value."
  ;; stolen from comp.lang.lisp article <k7727i3s.fsf@comcast.net> by
  ;; "prunesquallor@comcast.net"
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defvar ,variable-name)
     (setf (documentation ',variable-name 'variable)
           ,documentation)))

(defvar-unbound *saw-backslash*
  "Whether we have to re-process an \L or \U because it closes several
scopes.")

(defvar-unbound *pair-level*
  "")

(defvar-unbound *stream*
  "Bound to the stream which is read from while parsing a string.")

(defvar-unbound *start-char*
  "Bound to the opening outer delimiter while parsing a string.")

(defvar-unbound *term-char*
  "Bound to the closing outer delimiter while parsing a string.")

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-INTERPOL-SYNTAX.")

(defvar-unbound *readtable-copy*
  "Bound to the current readtable if it has to be temporarily
modified.")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-interpol/")

(let ((exported-symbols-alist
        (loop for symbol being the external-symbols of :cloture.interpol
              collect (cons symbol
                            (concatenate 'string
                                         "#"
                                         (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))

(define-condition simple-reader-error (simple-condition reader-error)
  ()
  (:documentation "A reader error which can be signalled by ERROR."))

(defmacro signal-reader-error (format-control &rest format-arguments)
  "Like ERROR but signals a SIMPLE-READER-ERROR for the stream
*STREAM*."
  `(error 'simple-reader-error
          :stream *stream*
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defun string-list-to-string (string-list)
  "Concatenates a list of strings to one string."
  ;; this function was originally provided by JP Massar for CL-PPCRE;
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-array total-size :element-type 'character))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun get-end-delimiter (start-delimiter delimiters &key errorp)
  "Find the closing delimiter corresponding to the opening delimiter
START-DELIMITER in a list DELIMITERS which is formatted like
*OUTER-DELIMITERS*. If ERRORP is true, signal an error if none was
found, otherwise return NIL."
  (loop for element in delimiters
        if (eql start-delimiter element)
          do (return-from get-end-delimiter start-delimiter)
        else if (and (consp element)
                     (char= start-delimiter (car element)))
               do (return-from get-end-delimiter (cdr element)))
  (when errorp
    (signal-reader-error "~S not allowed as a delimiter here" start-delimiter)))

(declaim (inline make-collector))
(defun make-collector ()
  "Create an empty string which can be extended by
VECTOR-PUSH-EXTEND."
  (make-array 0
              :element-type 'character
              :fill-pointer t
              :adjustable t))

(declaim (inline make-char-from-code))
(defun make-char-from-code (number)
  "Create character from char-code NUMBER. NUMBER can be NIL which is
interpreted as 0."
  ;; Only look at rightmost eight bits in compliance with Perl
  (let ((code (logand #o377 (or number 0))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-reader-error "No character for char-code #x~X"
                             number))))

(declaim (inline lower-case-p*))
(defun lower-case-p* (char)
  "Whether CHAR is a character which has case and is lowercase."
  (or (not (both-case-p char))
      (lower-case-p char)))

(defmacro read-char* ()
  "Convenience macro because we always read from the same string with
the same arguments."
  `(read-char *stream* t nil t))

(defmacro peek-char* ()
  "Convenience macro because we always peek at the same string with
the same arguments."
  `(peek-char nil *stream* t nil t))

(declaim (inline copy-readtable*))
(defun copy-readtable* ()
  "Returns a copy of the readtable which was current when
INTERPOL-READER was invoked. Memoizes its result."
  (or *readtable-copy*
      (setq *readtable-copy* (copy-readtable))))

(declaim (inline nsubvec))
(defun nsubvec (sequence start &optional (end (length sequence)))
  "Return a subvector by pointing to location in original vector."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

;;; define some aliases
(loop for (alias . name) in '(("LINE FEED" . "LINE FEED \(LF)")
                              ("FORM FEED" . "FORM FEED \(FF)")
                              ("CARRIAGE RETURN" . "CARRIAGE RETURN \(CR)")
                              ("NEXT LINE" . "NEXT LINE \(NEL)")
                              ("LF" . "LINE FEED \(LF)")
                              ("FF" . "FORM FEED \(FF)")
                              ("CR" . "CARRIAGE RETURN \(CR)")
                              ("NEL" . "NEXT LINE \(NEL)")
                              ("ZWNJ" . "ZERO WIDTH NON-JOINER")
                              ("ZWJ" . "ZERO WIDTH JOINER")
                              ("BYTE ORDER MARK" . "ZERO WIDTH NO-BREAK SPACE")
                              ("BOM" . "BYTE ORDER MARK")
                              ("HORIZONTAL TABULATION" . "CHARACTER TABULATION")
                              ("VERTICAL TABULATION" . "LINE TABULATION")
                              ("FILE SEPARATOR" . "INFORMATION SEPARATOR FOUR")
                              ("GROUP SEPARATOR" . "INFORMATION SEPARATOR THREE")
                              ("RECORD SEPARATOR" . "INFORMATION SEPARATOR TWO")
                              ("UNIT SEPARATOR" . "INFORMATION SEPARATOR ONE")
                              ("PARTIAL LINE DOWN" . "PARTIAL LINE FORWARD")
                              ("PARTIAL LINE UP" . "PARTIAL LINE BACKWARD"))
      for existing-char = (character-named name)
      when existing-char
        do (setf (gethash (canonicalize-name alias) *unicode-aliases*) existing-char))

(defun read-while (predicate &key max)
  "Reads characters from *STREAM* while PREDICATE returns a true value
for each character. Returns at most MAX characters if MAX is true."
  (when (eql max 0)
    (return-from read-while ""))
  (let ((collector (make-collector)))
    (loop for count of-type fixnum from 1
          for c = (peek-char*)
          while (and (or (not max)
                         (<= count max))
                     c
                     (funcall predicate c))
          do (vector-push-extend (read-char*) collector)
          finally (return collector))))

(declaim (inline get-number))
(defun get-number (&key (radix 10) max)
  "Reads and consumes the number *STREAM* is currently looking at and
returns it. Returns NIL if no number could be identified.  RADIX is
used as in PARSE-INTEGER. If MAX is not NIL we'll read at most the
next MAX characters."
  (parse-integer (read-while (lambda (c)
                               (digit-char-p c radix))
                             :max max)
                 :radix radix
                 :junk-allowed t))

(defun resolve-unicode-name (name)
  "Tries to return a character which was encoded as \\N<NAME>."
  (or (character-named name)
      (gethash (canonicalize-name name) *unicode-aliases*)))

(defun get-char-from-unicode-name ()
  "Parses and returns a named character after \"\\N\" has already been
read.  This function reads from *STREAM*."
  (let ((next-char (read-char*)))
    (unless (char= next-char #\{)
      (signal-reader-error "Expected { after \\N"))
    (let ((name (read-while (lambda (c)
                              (and (char/= c #\})
                                   (char/= c *term-char*))))))
      (let ((next-char (read-char*)))
        (unless (char= next-char #\})
          (signal-reader-error "Expected } after Unicode character name")))
      (or (resolve-unicode-name name)
          (signal-reader-error "Could not find character with name '~A'"
                               name)))))

(defun unescape-char (regex-mode)
  "Convert the characters(s) on *STREAM* following a backslash into a
character which is returned. This function is to be called when the
backslash has already been consumed."
  (let ((chr (read-char*)))
    ;; certain escape sequences are left as is when in regex mode
    (when (or (and (eq regex-mode :in-char-class)
                   (find chr "pPwWsSdD" :test #'char=))
              (and (eq regex-mode t)
                   (find chr "kpPwWsSdDbBAZz" :test #'char=)))
      (return-from unescape-char
        (concatenate 'string "\\" (string chr))))
    (let ((result
            (case chr
              ((#\N)
               ;; named Unicode chars
               (get-char-from-unicode-name))
              ((#\c)
               ;; \cx means control-x
               (when (char= (peek-char*) *term-char*)
                 (signal-reader-error "String ended after \\c"))
               (code-char (logxor #x40
                                  (char-code (char-upcase (read-char*))))))
              ((#\x)
               (cond ((char= (peek-char*) #\{)
                      ;; "wide" hex char, i.e. hexadecimal number is
                      ;; enclosed in curly brackets
                      (read-char*)
                      (prog1
                          (let ((code (or (get-number :radix 16)
                                          ;; allow for empty string
                                          0)))
                            (or (and (< code char-code-limit)
                                     (code-char code))
                                (signal-reader-error
                                 "No character for char-code #x~X" code)))
                        (unless (char= (peek-char*) #\})
                          (signal-reader-error "Expected } after hex code"))
                        (read-char*)))
                     (t
                      ;; \x should be followed by a hexadecimal char
                      ;; code, two digits or less; note that it is
                      ;; OK if \x is followed by zero digits
                      (make-char-from-code (get-number :radix 16 :max 2)))))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (cond ((and (eq regex-mode t)
                           (char/= chr #\0))
                      ;; leave as is if we're in regex mode (and not
                      ;; within in a character class)
                      (concatenate 'string "\\" (string chr)))
                     ((or (char= chr #\8)
                          (char= chr #\9))
                      ;; outside of regex mode "\8" is "8" (in regex
                      ;; mode it is read like "\08"...)
                      chr)
                     (t
                      (unread-char chr *stream*)
                      ;; now \x should be followed by an octal char
                      ;; code, three digits or less
                      (make-char-from-code (get-number :radix 8 :max 3)))))
              ((#\Newline)
               (read-while
                (lambda (c)
                  (or (char= c #\Space)
                      (not (or (graphic-char-p c) (char= c #\Newline))))))
               "")
              ;; the following five character names are
              ;; 'semi-standard' according to the CLHS but I'm not
              ;; aware of any implementation that doesn't implement
              ;; them
              ((#\t)
               #\Tab)
              ((#\n)
               #\Newline)
              ((#\r)
               #\Return)
              ((#\f)
               #\Page)
              ((#\b)
               #\Backspace)
              ((#\a)
               (code-char 7))                  ; ASCII bell
              ((#\e)
               (code-char 27))                 ; ASCII escape
              (otherwise
               ;; all other characters aren't affected by a backslash
               chr))))
      (cond ((and (characterp result)
                  ;; some characters must be 'protected' from CL-PPCRE
                  (or (and (eq regex-mode :in-char-class)
                           (find result "\\^[]-" :test #'char=))
                      (and (eq regex-mode t)
                           (find result "\\^[]-.$|()*+?" :test #'char=))))
             (concatenate 'string "\\" (string result)))
            (t result)))))

(declaim (inline normal-name-char-p)
         (inline never-name-char-p))

(defun normal-name-char-p (c)
  (and c (or (alphanumericp c)
             (member c '(#\_ #\- #\+ #\*)))))

(defun never-name-char-p (c)
  (or (not c)
      (get-macro-character c)
      (member c '(#\$ #\@))))

(defvar quell-warnings-form
  #+sbcl '(declare (optimize (sb-ext:inhibit-warnings 3)))
  #-sbcl nil
  "A declaration form to quiet warnings about unbound variables
  within a lexical environment.")

(defun read-longest-name ()
  (coerce
   (loop until (never-name-char-p (peek-char nil *stream* nil nil t))
         collect (read-char*))
   'string))

(defun read-optional-delimited ()
  "Read the stuff following an optional delimiter, returning a form
that tries to deal correctly with lexical variables."
  (flet ((try-pos (name i form)
           (let ((ostr (gensym)))
             `(handler-case
                  (with-output-to-string (,ostr)
                    (princ ,(read-from-string (subseq name 0 i)) ,ostr)
                    (princ ,(subseq name i) ,ostr)
                    ,ostr)
                (unbound-variable () ,form)))))

    (loop
      with name = (read-longest-name)
      with form = `(error ,(format nil "Interpolation error in ~s~%" name))
      with ostr = (gensym)
      for i = (position-if-not #'normal-name-char-p name)
        then (position-if-not #'normal-name-char-p name :start (1+ i))

      unless i
        return `(let () ,quell-warnings-form
                  (handler-case
                      (with-output-to-string (,ostr)
                        (princ ,(read-from-string name) ,ostr)
                        ,ostr)
                    (unbound-variable () ,form)))

      if (> i 0)
        do (setq form (try-pos name i form))

      if  (< i (length name))
        do (setq form (try-pos name (1+ i) form)))))

(declaim (inline read-form))
(defun read-form (&key (recursive-p t))
  "Reads and returns one or more Lisp forms from *STREAM* if the
character we're looking at is a valid inner delimiter. Otherwise
returns NIL."
  (let* ((start-delimiter (peek-char*))
         (end-delimiter (get-end-delimiter start-delimiter *inner-delimiters*)))
    (cond ((null end-delimiter)
           (if *optional-delimiters-p*
               (read-optional-delimited)
               nil))
          (t
           `(progn
              ,@(progn
                  (read-char*)
                  (let ((*readtable* (copy-readtable*)))
                    ;; temporarily change the readtable
                    (set-syntax-from-char end-delimiter #\))
                    (read-delimited-list end-delimiter *stream* recursive-p))))))))

(defun read-format-directive ()
  "Reads and returns a format directive (as a string) along with one
  or more lisp forms (as per read-form)."
  (let ((format-directive (make-collector)))
    (labels ((read-quoted-char ()
               (if (char= #\' (peek-char*))
                   (progn
                     (vector-push-extend (read-char*) format-directive)
                     (vector-push-extend (read-char*) format-directive)
                     t)
                   nil))
             (read-integer ()
               (if (member (peek-char*) '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                   (progn
                     (vector-push-extend (read-char*) format-directive)
                     (loop while (member (peek-char*) '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                           do (vector-push-extend (read-char*) format-directive))
                     t)
                   nil))
             (read-modifier ()
               (loop repeat 2
                     with found = nil
                     when (member (peek-char*) '(#\@ #\:))
                       do (vector-push-extend (read-char*) format-directive)
                       and do (setf found t)
                     finally (return found)))
             (read-comma ()
               (if (char= #\, (peek-char*))
                   (progn
                     (vector-push-extend (read-char*) format-directive)
                     t)
                   nil))
             (read-v ()
               (if (char-equal #\v (peek-char*))
                   (progn
                     (vector-push-extend (read-char*) format-directive)
                     t)
                   nil)))
      (loop
        while (or (read-quoted-char)
                  (read-integer)
                  (read-v)
                  (read-comma))
        finally (read-modifier)
        finally (vector-push-extend (read-char*) format-directive))
      format-directive)))

(defun interpol-reader (*stream* char arg &key (recursive-p t))
  "The actual reader function for the 'sub-character' #\?.

This function can be used directly outside of a read table by passing `recursive-p` as NIL.

"
  (declare (ignore arg char))
  (let ((*start-char* (read-char*))
        ;; REGEX-MODE is true if we're in regular expression mode; it
        ;; can have one of the values :START-OF-CHAR-CLASS,
        ;; :START-OF-NEGATED-CHAR-CLASS, or :IN-CHAR-CLASS if we're
        ;; inside of a character class or just about to start one -
        ;; otherwise the value is T
        regex-mode
        ;; EXTENDED-MODE is true if we're in extended regular
        ;; expression mode
        extended-mode)
    (when (char-equal *start-char* #\r)
      (setq regex-mode t
            *start-char* (read-char*)))
    (when (char-equal *start-char* #\x)
      (setq extended-mode t
            *start-char* (read-char*)))
    (when (and (not regex-mode)
               (find *start-char* *regex-delimiters* :test #'char=))
      (setq regex-mode t))
    (unless regex-mode
      (setq extended-mode nil))
    (let ((*term-char* (get-end-delimiter *start-char*
                                          *outer-delimiters*
                                          :errorp t))
          (*pair-level* 0)
          (*inner-delimiters* (if regex-mode
                                  (intersection *inner-delimiters*
                                                '((#\{ . #\}))
                                                :test #'equal)
                                  *inner-delimiters*))
          *saw-backslash*
          *readtable-copy*)
      (prog1
          (inner-reader regex-mode extended-mode nil nil :recursive-p recursive-p)
        ;; consume the closing outer delimiter
        (read-char*)))))

(defun inner-reader (regex-mode extended-mode quote-mode case-mode &key (recursive-p t))
  "Helper function for INTERPOL-READER which does all the work. May
call itself recursively."
  ;; REGEX-MODE and EXTENDED-MODE as described above; QUOTE-MODE is
  ;; true if we're inside a \Q scope; CASE-MODE is true if we're
  ;; inside a \L or \U scope
  (let* ((string-stream (gensym)) ;; the string stream
         ;; we use for WITH-OUTPUT-TO-STRING
         ;; if this is not a constant string
         (collector (make-collector)) ;; we collect
         ;; characters into this
         ;; extentable string
         result ;; a list of all characters, strings, and forms
         ;; so far (in reverse order while withing the loop)
         handle-next-char)
    (block main-loop ;; we need this name so we can leave the LOOP below
      (flet ((compute-result ()
               ;; local function used to leave the loop and compute
               ;; the final RESULT
               (setq result
                     (nreverse
                      (if (plusp (length collector))
                          ;; add COLLECTOR if it's not empty
                          (cons collector result)
                          result)))
               (return-from main-loop))
             (parse-with-case-mode (action-name)
               ;; local function used to read while in a \U or \L scope
               (let ((string-to-modify
                       ;; read until \E, \L, \U, or end of string
                       (inner-reader regex-mode extended-mode regex-mode t)))
                 (if (stringp string-to-modify)
                     ;; modify directly if constant string
                     (funcall action-name string-to-modify)
                     ;; otherwise create a form to do that at run time
                     `(write-string
                       (,action-name ,string-to-modify)
                       ,string-stream)))))
        (loop
          (let ((next-char (read-char*)))
            (when regex-mode
              ;; when in regex mode make sure where we are with
              ;; respect to character classes
              (setq regex-mode
                    (case next-char
                      ((#\[)
                       (ecase regex-mode
                         ((:start-of-char-class
                           :start-of-negated-char-class
                           :in-char-class) :in-char-class)
                         ((t) :start-of-char-class)))
                      ((#\^)
                       (ecase regex-mode
                         ((:start-of-char-class) :start-of-negated-char-class)
                         ((:start-of-negated-char-class
                           :in-char-class) :in-char-class)
                         ((t) t)))
                      ((#\])
                       (ecase regex-mode
                         ((:start-of-char-class
                           :start-of-negated-char-class) :in-char-class)
                         ((:in-char-class t) t)))
                      (otherwise
                       (ecase regex-mode
                         ((:start-of-char-class
                           :start-of-negated-char-class
                           :in-char-class) :in-char-class)
                         ((t) t))))))
            (when (and (char= next-char *start-char*)
                       (char/= *start-char* *term-char*))
              ;; if we see, say, #\( and our closing delimiter is #\)
              ;; we increment *PAIR-LEVEL* so the parentheses can next
              ;; without ending the string
              (incf *pair-level*))
            (let ((interpolation
                    (cond ((and (char= next-char *term-char*)
                                (plusp *pair-level*))
                           ;; although this is the outer closing
                           ;; delimiter we don't stop parsing because
                           ;; we're insided a nested pair of
                           ;; bracketing characters
                           (decf *pair-level*)
                           *term-char*)
                          ((char= next-char *term-char*)
                           ;; now we really stop - but we don't
                           ;; consume the closing delimiter because
                           ;; we may need it again to end another
                           ;; scope
                           (unread-char next-char *stream*)
                           (compute-result))
                          (t
                           (case next-char
                             ((#\L)
                              (cond ((not *saw-backslash*)
                                     ;; a normal #\L, no 'pending'
                                     ;; backslash
                                     #\L)
                                    (case-mode
                                     ;; a backslashed #\L which
                                     ;; we've seen before but we
                                     ;; still have to close at
                                     ;; least one \Q/\L/\E scope
                                     (unread-char #\L *stream*)
                                     (compute-result))
                                    (t
                                     ;; all scopes are closed, now
                                     ;; read and downcase 'till \E
                                     ;; or somesuch
                                     (setq *saw-backslash* nil)
                                     (parse-with-case-mode 'string-downcase))))
                             ((#\U)
                              ;; see comments for #\L above
                              (cond ((not *saw-backslash*)
                                     #\U)
                                    (case-mode
                                     (unread-char #\U *stream*)
                                     (compute-result))
                                    (t
                                     (setq *saw-backslash* nil)
                                     (parse-with-case-mode 'string-upcase))))
                             ((#\Space #\Tab #\Linefeed #\Return #\Page)
                              (cond ((and extended-mode
                                          (not (eq regex-mode :in-char-class)))
                                     ;; in extended mode (if not in
                                     ;; a character class)
                                     ;; whitespace is removed
                                     "")
                                    (t next-char)))
                             ((#\()
                              (cond ((and (eq regex-mode t)
                                          (null quote-mode)
                                          (char/= *term-char* #\?)
                                          (eql (peek-char*) #\?))
                                     ;; this could start an
                                     ;; embedded comment in regex
                                     ;; mode (and we're /not/
                                     ;; inside of a \Q scope or a
                                     ;; character class)
                                     (read-char*)
                                     (cond ((and (char/= *term-char* #\#)
                                                 (eql (peek-char*) #\#))
                                            ;; yes, it's a
                                            ;; comment, so consume
                                            ;; characters 'till #\)
                                            (read-while
                                             (lambda (char)
                                               (and (char/= char #\))
                                                    (char/= char *term-char*))))
                                            (cond ((char= (read-char*) *term-char*)
                                                   (signal-reader-error
                                                    "Incomplete regex comment starting with '(#'"))
                                                  ((not (digit-char-p (peek-char*) 16))
                                                   "")
                                                  ;; special case
                                                  ;; if next
                                                  ;; character
                                                  ;; could
                                                  ;; potentially
                                                  ;; continue an
                                                  ;; octal or
                                                  ;; hexadecimal
                                                  ;; representation
                                                  (t "(?:)")))
                                           ;; no, wasn't a comment
                                           (t "(?")))
                                    (t #\()))
                             ((#\#)
                              (cond ((and (eq regex-mode t)
                                          extended-mode
                                          (null quote-mode))
                                     ;; we're in extended regex
                                     ;; mode and not inside of a \Q
                                     ;; scope or a character class,
                                     ;; so this is a comment and we
                                     ;; consume it 'till #\Newline
                                     ;; or *TERM-CHAR*
                                     (read-while
                                      (lambda (char)
                                        (and (char/= char #\Newline)
                                             (char/= char *term-char*))))
                                     (when (char= (peek-char*) #\Newline)
                                       (read-char*))
                                     (cond ((not (digit-char-p (peek-char*)
                                                               16))
                                            "")
                                           ;; special case, see above
                                           (t "(?:)")))
                                    (t #\#)))
                             ((#\\)
                              (case (peek-char*)
                                ((#\Q)
                                 ;; \Q - start a new quote scope
                                 (read-char*)
                                 (let ((string-to-quote
                                         (inner-reader regex-mode
                                                       extended-mode
                                                       t case-mode)))
                                   (if (stringp string-to-quote)
                                       ;; if we got a constant string
                                       ;; we modify it directly
                                       (quote-meta-chars string-to-quote)
                                       ;; otherwise we expand into code
                                       `(write-string
                                         (quote-meta-chars ,string-to-quote)
                                         ,string-stream))))
                                ((#\L)
                                 ;; \L - start a new case-modifying
                                 ;; scope
                                 (cond (case-mode
                                        ;; if we're already in
                                        ;; this mode we have to
                                        ;; end all previous scopes
                                        ;; first - we set
                                        ;; *SAW-BACKSLASH* to T so
                                        ;; the #\L is read until
                                        ;; all scopes are finished
                                        (setq *saw-backslash* t)
                                        (compute-result))
                                       (t
                                        ;; all scopes are closed, now
                                        ;; read and downcase 'till \E
                                        ;; or somesuch
                                        (setq *saw-backslash* nil)
                                        (read-char*)
                                        (parse-with-case-mode 'string-downcase))))
                                ((#\U)
                                 ;; see comments for #\L above
                                 (cond (case-mode
                                        (setq *saw-backslash* t)
                                        (compute-result))
                                       (t
                                        (setq *saw-backslash* nil)
                                        (read-char*)
                                        (parse-with-case-mode 'string-upcase))))
                                ((#\E)
                                 ;; \E - ends exactly one scope
                                 (read-char*)
                                 (if (or quote-mode case-mode)
                                     (compute-result)
                                     ""))
                                ((#\l)
                                 ;; \l - downcase next character
                                 (read-char*)
                                 ;; remember that we have to do this
                                 (setq handle-next-char :downcase)
                                 nil)
                                ((#\u)
                                 ;; \u - upcase next character
                                 (read-char*)
                                 ;; remember that we have to do this
                                 (setq handle-next-char :upcase)
                                 nil)
                                (otherwise
                                 ;; otherwise this is a
                                 ;; backslash-escaped character
                                 (unescape-char regex-mode))))
                             ((#\~)
                              ;; #\~ - might be an inline format directive
                              (if *interpolate-format-directives*
                                  `(format ,string-stream
                                           ,(concatenate 'string "~" (read-format-directive))
                                           ,@(let ((form (read-form :recursive-p recursive-p)))
                                               (if form
                                                   (list form)
                                                   '())))
                                  #\~))
                             ((#\$)
                              ;; #\$ - might be an interpolation
                              (let ((form (read-form :recursive-p recursive-p)))
                                (cond ((null form)
                                       ;; no, just dollar sign
                                       #\$)
                                      (handle-next-char
                                       ;; yes, and we have to
                                       ;; modify the first
                                       ;; character
                                       (prog1
                                           (let ((string (gensym)))
                                             `(let ((,string (format nil "~A"
                                                                     ,form)))
                                                (when (plusp (length ,string))
                                                  (setf (char ,string 0)
                                                        (,(if (eq handle-next-char
                                                                  :downcase)
                                                              'char-downcase
                                                              'char-upcase)
                                                         (char ,string 0))))
                                                (write-string ,string ,string-stream)))
                                         (setq handle-next-char nil)))
                                      (t
                                       ;; no modification, just
                                       ;; insert a form to PRINC
                                       ;; this interpolation
                                       `(princ ,form ,string-stream)))))
                             ((#\@)
                              ;; #\Q - might be an interpolation
                              (let ((form (read-form :recursive-p recursive-p))
                                    (element (gensym))
                                    (first (gensym)))
                                (cond ((null form)
                                       ;; no, just at-sign
                                       #\@)
                                      (handle-next-char
                                       ;; yes, and we have to
                                       ;; modify the first
                                       ;; character
                                       (prog1
                                           (let ((string (gensym)))
                                             `(loop for ,first = t then nil
                                                    for ,element in ,form
                                                    unless ,first do
                                                      (princ *list-delimiter*
                                                             ,string-stream)
                                                    if ,first do
                                                      (let ((,string
                                                              (format nil "~A"
                                                                      ,element)))
                                                        (when (plusp (length ,string))
                                                          (setf (char ,string 0)
                                                                (,(if (eq handle-next-char
                                                                          :downcase)
                                                                      'char-downcase
                                                                      'char-upcase)
                                                                 (char ,string 0))))
                                                        (write-string ,string ,string-stream))
                                                    else do
                                                      (princ ,element ,string-stream)))
                                         (setq handle-next-char nil)))
                                      (t
                                       ;; no modification, just
                                       ;; insert a form to PRINC
                                       ;; this interpolated list
                                       ;; (including the list
                                       ;; delimiters inbetween)
                                       `(loop for ,first = t then nil
                                              for ,element in ,form
                                              unless ,first do (princ *list-delimiter*
                                                                      ,string-stream)
                                                do (princ ,element ,string-stream))))))
                             ;; just a 'normal' character
                             (otherwise next-char))))))
              (when interpolation
                ;; INTERPOLATION is NIL if we just saw #\l or #\u
                (when (and handle-next-char
                           (consp interpolation)
                           (eq (first interpolation)
                               'write-string))
                  ;; if we have to upcase or downcase the following
                  ;; character and we just collected a form (from a
                  ;; \Q/\L/\U scope) we have to insert code for the
                  ;; modification
                  (setf (second interpolation)
                        (let ((string (gensym)))
                          `(let ((,string ,(second interpolation)))
                             (when (plusp (length ,string))
                               (setf (char ,string 0)
                                     (,(if (eq handle-next-char :downcase)
                                           'char-downcase
                                           'char-upcase)
                                      (char ,string 0))))
                             ,string)))
                  (setq handle-next-char nil))
                (cond ((characterp interpolation)
                       ;; add one character to COLLECTOR and handle
                       ;; it according to HANDLE-NEXT-CHAR
                       (vector-push-extend (case handle-next-char
                                             ((:downcase)
                                              (setq handle-next-char nil)
                                              (char-downcase interpolation))
                                             ((:upcase)
                                              (setq handle-next-char nil)
                                              (char-upcase interpolation))
                                             (otherwise
                                              interpolation))
                                           collector))
                      ((stringp interpolation)
                       ;; add a string to COLLECTOR and handle its
                       ;; first character according to
                       ;; HANDLE-NEXT-CHAR
                       (loop for char across interpolation
                             do (vector-push-extend (case handle-next-char
                                                      ((:downcase)
                                                       (setq handle-next-char nil)
                                                       (char-downcase char))
                                                      ((:upcase)
                                                       (setq handle-next-char nil)
                                                       (char-upcase char))
                                                      (otherwise
                                                       char))
                                                    collector)))
                      ((plusp (length collector))
                       ;; add code (to be executed at runtime) but
                       ;; make sure to empty COLLECTOR first
                       (push collector result)
                       (push interpolation result)
                       ;; reset collector
                       (setf collector (make-collector)))
                      (t
                       ;; same but COLLECTOR is empty
                       (push interpolation result)))))))))
    (if (every #'stringp result)
        ;; if all elements of RESULT are strings we can return a
        ;; constant string
        (string-list-to-string result)
        ;; otherwise we have to wrap the PRINCs emitted above into a
        ;; WITH-OUTPUT-TO-STRING form
        `(with-output-to-string (,string-stream)
           ,@(loop for interpolation in result
                   if (stringp interpolation)
                     collect `(write-string ,interpolation ,string-stream)
                   else
                     collect interpolation)))))

(defun %enable-interpol-syntax (&key (modify-*readtable* nil))
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (unless modify-*readtable*
    (push *readtable*
          *previous-readtables*)
    (setq *readtable* (copy-readtable)))
  (set-dispatch-macro-character #\# #\? #'interpol-reader)
  (values))

(defun %disable-interpol-syntax ()
  "Internal function used to restore previous readtable."
  (if *previous-readtables*
      (setq *readtable* (pop *previous-readtables*))
      (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-interpol-syntax (&rest %enable-interpol-syntax-args)
  "Enable CL-INTERPOL reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-interpol-syntax ,@%enable-interpol-syntax-args)))

(defmacro disable-interpol-syntax ()
  "Restore readtable which was active before last call to
ENABLE-INTERPOL-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-interpol-syntax)))

(defreadtable :interpol-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'interpol-reader))
