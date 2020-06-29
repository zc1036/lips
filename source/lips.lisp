
;;;; lips the non-dumb text macro system

(defpackage :lips
  (:use :cl)
  (:export :process-stream :*finish-hooks*
           :*paragraph-end* :*paragraph-begin*
           :*in-paragraph* :reset-paragraph :in-paragraph
           :*use-smart-quotes*))

(defpackage :lips-user
  (:use :cl))

(in-package :lips-user)

(declaim (ftype (function (t) t) include-defs))

(in-package :lips)

(defparameter *paragraph-begin* nil)
(defparameter *paragraph-end* nil)
(defparameter *finish-hooks* nil)

(asdf:load-system :unix-opts)

(defparameter *hot-char* #\~)

(defparameter *original-readtable* *readtable*)

(defun princ-if (x)
   "If the given object is a function, prints the return value when
    the return value is non-NIL; otherwise, prints the object if it is
    non-NIL."
    (let ((res (etypecase x
                 (function (funcall x))
                 (t x))))
      (when res (princ res))))

(defparameter *in-paragraph* nil)

(defun reset-paragraph ()
  (setf *in-paragraph* nil))

(defun in-paragraph ()
  *in-paragraph*)

(defparameter *whitespace* (list #\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))

(defparameter *use-smart-quotes* nil)

(defun update-quote-char (char last-char-was-space)
  (if *use-smart-quotes*
      (case char
        (#\'
         (if last-char-was-space
             (format t "‘")
             (format t "’")))
        (#\"
         (if last-char-was-space
             (format t "“")
             (format t "”")))
        (t char))
      char))

(defun process-stream (stream)
    ;; If the first line of the file is a hashbang, discard the line.
    (let ((first-char (read-char stream nil)))
        (when (and first-char (char= #\# first-char) (char= #\! (peek-char nil stream nil #\.)))
            (read-line stream nil)
            (setf first-char (read-char stream nil)))
        ;; Loop until there are no more characters in the
        ;; input stream.
        (loop
           for char = first-char then (read-char stream nil)
           with last-char-was-newline = t
           with last-char-was-space = t
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
                (when (not *in-paragraph*)
                  (princ-if *paragraph-begin*)
                  (setf *in-paragraph* t))
                (setf last-char-was-newline nil)
                (if (char= (peek-char nil stream t) *hot-char*)
                    (progn
                      (write-char *hot-char*)
                      (read-char stream nil))
                    (let ((*package* (find-package :lips-user))
                          (*standard-input* stream))
                      (multiple-value-bind (ret halt) (eval (macroexpand (read stream)))
                        (when halt
                          (when *in-paragraph*
                            (princ-if *paragraph-end*)
                            (setf *in-paragraph* nil))
                          (princ-if ret)
                          (return halt))
                        (princ-if ret)))))
               ((char= char #\newline)
                (write-char char)
                (when (and last-char-was-newline *in-paragraph*)
                  (princ-if *paragraph-end*)
                  (setf *in-paragraph* nil))
                (setf last-char-was-newline t))
               (t
                (when (not *in-paragraph*)
                  (princ-if *paragraph-begin*)
                  (setf *in-paragraph* t))
                (princ-if (update-quote-char char
                                             (or last-char-was-space
                                                 last-char-was-newline)))
                (setf last-char-was-newline nil)
                (setf last-char-was-space (member char *whitespace*))))
           finally
             (when *in-paragraph*
               (princ-if *paragraph-end*))
             (return nil))))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defparameter *include-defs* nil)

(defun include-defs-parser (filename)
  (push filename *include-defs*))

(defun main ()
  (opts:define-opts
      (:name :help
             :description "print this help text"
             :short #\h
             :long "help")
      (:name :include-defs
             :description "Include definitions before processing input"
             :short #\i
             :long "include-defs"
             :arg-parser #'include-defs-parser
             :meta-var "FILE"))

  (multiple-value-bind (options args) (opts:get-opts)
    (when-option (options :help)
      (opts:describe
       :prefix "example—program to demonstrate unix-opts library"
       :suffix "so that's how it works…"
       :usage-of "example.sh"
       :args     "[FREE-ARGS]")
      (return-from main))

    (let ((*package* (find-package :lips-user)))
      (mapcar #'lips-user::include-defs *include-defs*))

    (if args
        (loop for filename in args do
             (if (string= filename "-")
                 (process-stream *standard-input*)
                 (with-open-file (input-stream filename)
                   (let ((*standard-input* input-stream))
                     (let ((result (process-stream input-stream)))
                       (when result
                         (error "PROCESS-STREAM unexpectedly returned ~a" result)))
                     (mapc #'princ-if *finish-hooks*)))))
        (progn
          (process-stream *standard-input*)
          (mapc #'princ-if *finish-hooks*)))))

;;; Functions for use in text to be processed.

(in-package :lips-user)

(defun add-finish-hook (func)
    (push func lips:*finish-hooks*)
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
        (lips:process-stream input))
    (values))
