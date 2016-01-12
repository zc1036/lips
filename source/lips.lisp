
;;;; lips the non-dumb text macro system

(make-package :lips :use '(:cl))
(make-package :lips-user :use '(:cl))

(in-package :lips-user)

(defparameter *paragraph-separator* nil)

(in-package :lips)

(defparameter *hot-char* #\~)

(defparameter *original-readtable* *readtable*)

(defun princ-if (x)
"   If the given object is a function, prints the return value when
    non-NIL; otherwise, prints the object if it is non-NIL."
    (let ((res (etypecase x
                 (function (funcall x))
                 (t x))))
        (when res (princ res))))

;; Read characters until we get to a lisp form.
(defun process-stream (&optional (stream *standard-input*))
    ;; If the first line of the file is a hashbang, discard the line.
    (let ((first-char (read-char stream nil #\.)))
        (when (and (char= #\# first-char) (char= #\! (peek-char nil stream nil #\.)))
            (read-line stream nil)
            (setf first-char (read-char stream nil)))
        ;; Loop until there are no more characters in the
        ;; input stream.
        (loop
           for char = first-char then (read-char stream nil)
           with last-char-was-newline
           while char
           do
             ;; If the character is *hot-char*, then check if
             ;; it's escaped by peeking at the next character
             ;; in the stream. If not, just print it. If so,
             ;; go on.
             (cond
               ((char= char *hot-char*)
                ;; If it is escaped, then just print
                ;; *hot-char* and discard the extra one.
                ;; Otherwise, read an object, macroexpand
                ;; and evaluate it, and if it's not nil,
                ;; print it.
                (if (char= (peek-char nil stream t) *hot-char*)
                    (progn
                        (write-char *hot-char*)
                        (read-char stream nil))
                    (let* ((*package* (find-package :lips-user))
                           (obj (eval (macroexpand (read stream t)))))
                        (when obj
                            (princ obj)))))
               ((char= char #\newline)
                (write-char char)
                (when last-char-was-newline
                    (princ-if lips-user::*paragraph-separator*))
                (setf last-char-was-newline (not last-char-was-newline)))
               (t
                (setf last-char-was-newline nil)
                (write-char char))))))

(defparameter *finish-hooks* nil)

(defun main ()
    (process-stream)
    (mapc #'princ-if *finish-hooks*))

;;; Functions for use in text to be processed.

(in-package :lips-user)

(defun add-finish-hook (func)
    (push func lips::*finish-hooks*)
    (values))

;; To define functions without evaluating to the symbol name.
(defmacro defun-q (name args &body body)
    `(progn
         (defun ,name ,args ,@body)
         (values)))

;; To define values without evaluating to the symbol name.
(defmacro defparameter-q (name value)
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
    (with-open-file (input filename)
        (lips::process-stream input))
    (values))
