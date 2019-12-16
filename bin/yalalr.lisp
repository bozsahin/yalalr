;;;; ============================================================================
;;;; ==   The interface to the LALR parser of Mark Johnson
;;;; ==   -cem bozsahin, 2017-2019, Ankara
;;;; ============================================================================

(defparameter *ENDMARKER* '$) ; these are the globals of the lalrparser.lisp
(defparameter grammar nil)
(defparameter lexforms nil)
(defparameter lexicon nil)

(defparameter *lexer* nil) ; the lexer function to call

(defun which-yalalr ()
  "yalalr, version 2.0")

(defun welcome()
  (format t "~%===================================================")
  (format t "~%Welcome to ~d" (which-yalalr))
  (format t "~%an interface to LALR parser of Mark Johnson")
  (format t "~%---------------------------------------------------")
  (format t "~%Ready.")
  (format t "~%===================================================~%"))

(defmacro set-lexer (msg &optional (exec "lexer"))
  "exec must be an executable"
  `(progn (format t "~d~%" ,msg)
	  (setf *lexer* ,exec)))

(defmacro load-sdd (msg &optional (sdd nil))
  "include in sdd.lisp file your LALR grammar and its SDD functions"
  `(progn 
     (format t "~d~%" ,msg)
     (if ,sdd
       (progn (load "sdd.lisp") 
	      (make-lalrparser)
	      (format t "SDD loaded.~%LALR tables set.~%")))))

(defun help ()
  (format t "~%-put the lex analyzer in 'lexer' and your code generator in 'sdd.lisp'")
  (format t "~%-make sure the lexer takes a source file (say f) as input and outputs f.tokens wrapped in ()")
  (format t "~%-LALR's ACTION and GOTO tables are automatically generated when you load sdd.lisp")
  (format t "~%   if you update your grammar, you need to remake the tables. Use (make-lalrparser)")
  (format t "~%   or just exit yalalr and run it again.")
  (format t "~%-Call 'target-code' or 'ic-gen' with source code file to generate target code or IC")
  (format t "~2%Check out README.md, and sample workflows in examples directory."))



;;; lalrparser.lisp is not publicized as a package so we need to recreate
;;; grammars when we have more than one. lalr-parser function will be different in each case,
;;; but parse function is the same for our purposes. We will override the definition of that function below.

;;; to automatically generate the parser by LALR parser generator

(defun make-lalrparser ()
  "makes the lalr-parser function used by parse below"
  (compile (eval (make-parser grammar lexforms *ENDMARKER*)))) 

(defun parse (words)
  "Overriding lalrparser.lisp's parse function. We must do this to return any symbol 
   which is not in the 'lexicon' list to be returned as ID type."
  (labels ((lookup (word)
                   (cadr (assoc word lexicon)))
           (next-input ()
                       (let* ((word (pop words))
                              (cat (lookup word)))
                         (cond (cat (cons cat                     ; category if it exists, and
                                          (list cat word)))       ; value
			       ((typep word 'string)              ; a quoted multiword entry, make it Lisp string
				(cons 'ID (list 'ID (concatenate 'string "\"" word "\""))))
                               (t (cons 'ID (list 'ID word))))))  ; if not in lexicon, take as ID.
				
           (parse-error ()
                        (format nil "Error before ~A" words)))
    (lalr-parser #'next-input #'parse-error)))

(defun target-code (sourcecode)
  "calls lexer, which returns sourcecode.tokens, sticks in the end marker to lexically analyzed words, 
  to pass on to lalrparser's overridden parse function."
  (let* ((lexout (concatenate 'string sourcecode ".tokens"))
	 (tokens (run-program *lexer* (list sourcecode lexout) :wait t)))
    (with-open-file (s lexout :direction :input
		     :if-does-not-exist :error)
      (parse (append (read s) (list *ENDMARKER*))))))

(defmacro ic-gen (words)
  `(target-code ,words))
