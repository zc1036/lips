
;;;; lips the non-dumb text macro system

(defparameter *characters* '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0 #\1
                             #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B
                             #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
                             #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d
                             #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
                             #\v #\w #\x #\y #\z #\{ #\| #\} #\~ #\Space #\Newline))

(defun read-lips-char (stream char)
    (declare (ignore stream))

    (format t "HI! ~a ~%" char)

    t)

(defparameter lips-readtable (copy-readtable *readtable*))

(loop for char in *characters* do
     (set-macro-character char #'read-lips-char nil lips-readtable))

(let ((*readtable* lips-readtable))
    (with-open-file (input "hi" :direction :input)
        (loop while (read input nil))))
