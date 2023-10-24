
;;;; lips the non-dumb text macro system

(defpackage :lips
  (:use :cl)
  (:export :process-stream
           :*finish-hooks*
           :*paragraph-end* :*paragraph-begin*
           :*use-smart-quotes*
           :*hot-char*))

(defpackage :lips-user
  (:use :cl))

(in-package :lips-user)

(declaim (ftype (function (t) t) include-defs))

(in-package :lips)

(defparameter *paragraph-begin* nil)
(defparameter *paragraph-end* nil)
(defparameter *finish-hooks* nil)
(defparameter *hot-char* #\\)
(defparameter *silent-char* #\-)

(defun princ-if (x)
   "If the given object is a function, prints the return value when
    the return value is non-NIL; otherwise, prints the object if it is
    non-NIL."
    (let ((res (etypecase x
                 (function (funcall x))
                 (t x))))
      (when res (princ res))))

(defparameter *whitespace* (list #\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))

(defparameter *use-smart-quotes* nil)

(defparameter *left-quote-exceptions*
  '(#\{ #\( #\[ #\- #\– #\—))

(defun update-quote-char (char last-char)
  (let ((leftside (or (null last-char)
                      (member last-char *whitespace*)
                      (member last-char *left-quote-exceptions*))))
    (ecase char
      (#\'
       (if leftside
           #\‘
           #\’))
      (#\"
       (if leftside
           #\“
           #\”)))))

(defun call-or-write (object)
  (etypecase object
    (null (values))
    (function
     (funcall object))
    (string
     (write-string object))
    (t
     (format t "~a" object))))

(defmacro write-char-update-state (last-char in-paragraph allow-eof c &key (fake nil))
  (let ((c% (gensym)))
    `(progn
       (let ((,c% ,c))
         (when (and ,allow-eof
                     (not ,in-paragraph)
                     (not (member ,c% *whitespace*)))
           (call-or-write *paragraph-begin*)
           (setf ,in-paragraph t))

         (unless ,fake
           (write-char ,c%))

         (when (char= ,c% #\Newline)
           (when (and ,allow-eof ,in-paragraph (char= ,last-char #\Newline))
             (call-or-write *paragraph-end*)
             (setf ,in-paragraph nil)))

         (setf ,last-char ,c%)))))

(defmacro write-object-update-state (last-char in-paragraph allow-eof object &key (fake nil))
  (let ((evald% (gensym))
        (string% (gensym))
        (i% (gensym)))
    `(let* ((,evald% ,object)
            (,string% (format nil "~a" ,evald%)))
       (when ,evald%
         (loop for ,i% from 0 below (length ,string%)
            do (write-char-update-state ,last-char
                                        ,in-paragraph
                                        ,allow-eof
                                        (char ,string% ,i%)
                                        :fake ,fake))))))

(defparameter *internal-write-update-state* nil)

(defun read-interpolated-string (&optional arg-char end-char (allow-eof t) error-char)
  (let ((output-string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (output-stream output-string)
      (let ((*standard-output* output-stream)
            (end-char-nest-level 1)
            (last-char nil)
            (in-paragraph nil))
        (let ((*internal-write-update-state*
               (lambda (obj &optional fake)
                 (write-object-update-state last-char in-paragraph allow-eof obj :fake fake))))
          (loop
             for char = (read-char *standard-input* (not allow-eof))
             while char
             do
               (cond
                 ((and error-char (char= char error-char))
                  (error "Unexpected }"))
                 ((and *use-smart-quotes*
                       (or (char= char #\") (char= char #\')))
                  (write-char-update-state last-char in-paragraph allow-eof
                                           (update-quote-char char last-char)))
                 ((char= char *hot-char*)
                  (let ((next-char (peek-char)))
                    (cond
                      ;; escaped hot-char, like \\, or escaped opener or
                      ;; closer, like \}
                      ((or (char= next-char *hot-char*)
                           (and end-char (char= next-char end-char))
                           (and arg-char (char= next-char arg-char)))
                       (write-char-update-state last-char in-paragraph allow-eof (read-char)))
                      ((char= next-char *silent-char*)
                       ;; When *SILENT-CHAR* follows *HOT-CHAR*, evaluate the
                       ;; following form for side effects only.
                       (read-char)
                       (eval (macroexpand (read))))
                      ;; any other char, evaluate the lisp code
                      (t
                       (let ((result (eval (macroexpand (read)))))
                         (when result
                           (write-object-update-state last-char in-paragraph allow-eof result)))))))
                 ((and arg-char (char= char arg-char))
                  (incf end-char-nest-level)
                  (write-char-update-state last-char in-paragraph allow-eof char))
                 ((and end-char (char= char end-char))
                  (when (= 0 (decf end-char-nest-level))
                    (return))
                  (write-char-update-state last-char in-paragraph allow-eof char))
                 (t
                  (write-char-update-state last-char in-paragraph allow-eof char)))
             finally
               (setf last-char char))
          (when in-paragraph
            (call-or-write *paragraph-end*))
          (values output-string *internal-write-update-state*))))))

(defparameter *current-macro-body* nil)

(defun lips-user::read-macro-argument (&optional (arg-char #\{) (end-char #\}) macro-name)
  (loop
     for char = (read-char *standard-input*)
     do
       (cond
         ((member char *whitespace*)
          :do-nothing)
         ((char= char arg-char)
          (return (read-interpolated-string arg-char end-char nil)))
         (t
          (error "Unexpected character '~a' while reading argument for macro ~a"
                 char
                 (or macro-name *current-macro-body* ""))))))

(defparameter *lips-readtable* (copy-readtable *readtable*))

;; We need { to be a terminating macro character so that, in cases
;; like \blah{1}, READ will read the symbol |BLAH| instead of the symbol
;; |BLAH{1}|
;; We also want } to be a terminating macro char so that we can catch unbalanced
;; braces that are adjacent to words like "blah}".
(set-macro-character #\{ nil nil *lips-readtable*)
(set-macro-character #\} nil nil *lips-readtable*)

(defun process-stream (stream)
  (let ((*standard-input* stream)
        (*readtable* *lips-readtable*))
    (multiple-value-bind (result writer-fn) (read-interpolated-string nil nil t #\})
      (write-string result)
      writer-fn)))

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
             :description "Print this help text"
             :short #\h
             :long "help")
      (:name :include-defs
             :description "Include definitions before processing any input"
             :short #\i
             :long "include-defs"
             :arg-parser #'include-defs-parser
             :meta-var "FILE"))

  (multiple-value-bind (options args) (opts:get-opts)
    (when-option (options :help)
      (opts:describe
       :prefix "Lips Iwesome Preprocessor System"
       :usage-of "lips"
       :args     "[FILENAMES]")
      (return-from main))

    (let ((*package* (find-package :lips-user)))
      (mapcar #'lips-user::include-defs *include-defs*))

    (if args
        (loop for filename in args do
             (let ((writer (if (string= filename "-")
                               (process-stream *standard-input*)
                               (with-open-file (input-stream filename)
                                 (process-stream input-stream)))))
               (let ((*internal-write-update-state* writer))
                 (mapc #'princ-if *finish-hooks*))))
        (progn
          (let ((writer (process-stream *standard-input*)))
            (let ((*internal-write-update-state* writer))
              (mapc #'princ-if *finish-hooks*)))))))

;;; Functions for use in text to be processed.

(in-package :lips-user)

(defmacro macro (name args &body body)
  (let ((fn-name% (intern (concatenate 'string "$" (symbol-name name)) (symbol-package name)))
        (num-args (length args)))
    `(progn
       (defun ,fn-name% ,args
         (let ((*current-macro-body* ',name))
           ,@body))
       (define-symbol-macro ,name
           (apply #',fn-name% (loop for i from 0 below ,num-args collect
                                   (read-macro-argument #\{ #\} ',name))))
       nil)))

(defun add-finish-hook (func)
    (push func lips:*finish-hooks*)
    (values))

;; To define a symbol macro
(defmacro defsym (name args &body body)
  (let ((fn-name% (intern (concatenate 'string (symbol-name name) "%%")
                          (symbol-package name))))
    `(progn
       (defun ,fn-name% ,args
         ,@body)
       (define-symbol-macro ,name
           (let ((*current-macro-body* ',name))
             (,fn-name%)))
       nil)))

;; To define functions without evaluating to the symbol name.
(defmacro defn (name args &body body)
    `(progn
         (defun ,name ,args ,@body)
         (values)))

;; To define values without evaluating to the symbol name.
(defmacro defv (name value)
    `(progn
         (defparameter ,name ,value)
         (values)))

;; Setf quietly
(defmacro setv (place value)
    `(progn (setf ,place ,value) (values)))

;; Loads the file as lisp source code, that is, without requiring the
;; *hot-char* to evaluate lisp.
(defun include-defs (filename)
    (load filename :verbose nil :print nil)
    (values))

;; Treats "filename" as if its contents had appeared in the original
;; file at the position of "include-text".
(defun include (filename)
    (with-open-file (input filename)
        (lips:process-stream input))
    (values))

(defmacro $! (obj)
  `(progn (princ ,obj)
          nil))

(defmacro %! (&rest args)
  `(progn (format t ,@args)
          nil))

(defmacro $ (obj &optional fake)
  `(progn (funcall lips::*internal-write-update-state*
                   ,obj
                   ,fake)
          nil))

(defmacro % (&rest args)
  `(progn (funcall lips::*internal-write-update-state*
                   (format nil ,@args))
          nil))

(lips::main)
