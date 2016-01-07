
;;;; lips the non-dumb text macro system

(make-package :lips :use '(:cl))
(in-package :lips)

;; Adapted from http://cl-cookbook.sourceforge.net/os.html
(define-symbol-macro command-line-args
    (or 
     #+SBCL *posix-argv*  
     #+LISPWORKS system:*line-arguments-list*
     #+CMU extensions:*command-line-words*
     nil))

(defparameter *characters* '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0 #\1
                             #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B
                             #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
                             #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d
                             #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
                             #\v #\w #\x #\y #\z #\{ #\| #\} #\~ #\Space #\Newline))

(defparameter *hot-char* #\~)

(defparameter *original-readtable* *readtable*)

(defun princ-if (x)
    (let ((res (etypecase x
                 (function (funcall x))
                 (t x))))
        (when res (princ res))))

(defparameter *paragraph-separator* nil)

;; Read characters until we get to a lisp form.
(defun read-lips-char (stream read-char)
    ;; We print to an in-memory buffer to avoid the inefficiency of
    ;; printing one character at a time.
    (princ (with-output-to-string (stdout)
             (let ((*standard-output* stdout)
                   (last-char-was-newline nil))
                 ;; Loop until there are no more characters in the
                 ;; input stream.
                 (loop
                    for char = read-char then (read-char stream nil nil t)
                    while char
                    do
                      (when (not (eq char #\newline))
                          (setf last-char-was-newline nil))
                      ;; If the character is *hot-char*, then check if
                      ;; it's escaped by peeking at the next character
                      ;; in the stream. If not, just print it. If so,
                      ;; go on.
                      (cond
                        ((eq char *hot-char*)
                         ;; If it is escaped, then just print
                         ;; *hot-char* and discard the extra one. ;;
                         ;; Otherwise, read an object, macroexpand
                         ;; and evaluate it, and if it's not nil,
                         ;; print it.
                         (if (eq (peek-char nil stream t nil t) *hot-char*)
                             (progn
                                 (princ *hot-char*)
                                 (read-char stream nil nil t))

                             (let* ((*readtable* *original-readtable*)
                                    (*package* (find-package :lips))
                                    (obj (eval (macroexpand (read stream)))))
                                 (when (not (null obj))
                                     (princ obj)))))
                        ((eq char #\newline)
                         (princ char)
                         (when last-char-was-newline
                             (princ-if *paragraph-separator*))
                         (setf last-char-was-newline (not last-char-was-newline)))
                        (t
                         (setf last-char-was-newline nil)
                         (princ char))))))))

(defparameter lips-readtable (copy-readtable *readtable*))

(loop for char in *characters* do
     (set-macro-character char #'read-lips-char nil lips-readtable))

(defparameter *finish-hooks* nil)

(defun add-finish-hook (func)
    (push func *finish-hooks*)
    (values))

(defun main ()
    (let ((*readtable* lips-readtable))
        ;; (with-open-file (input (cadr command-line-args) :direction :input))
        (loop while (read *standard-input* nil))
        (mapc #'princ-if *finish-hooks*)))

;;; Functions for use in text to be processed.

;; To define functions without evaluating to the symbol name.
(defmacro fn (name args &body body)
    `(progn
         (defun ,name ,args ,@body)
         (values)))

;; To define values without evaluating to the symbol name.
(defmacro val (name value)
    `(progn
         (defparameter ,name ,value)
         (values)))

;; Setf quietly
(defmacro setf-q (place value)
    `(progn (setf ,place ,value) (values)))

;; Loads the file as lisp source code, that is, without requiring the
;; *hot-char* to evaluate lisp.
(defun include-defs (filename)
    (load filename :verbose nil :print nil)
    (values))

;; Treats "filename" as if its contents had appeared in the original
;; file at the position of "include-text".
(defun include-text (filename)
    (let ((*readtable* lips-readtable))
        (include-defs filename))
    (values))
