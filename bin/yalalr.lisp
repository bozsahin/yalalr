;;;; ============================================================================
;;;; ==   The interface to the LALR parser of Mark Johnson
;;;; ==   -cem bozsahin, 2017
;;;; ============================================================================

(defparameter *ENDMARKER* '$) ; these are the globals of lalrparser.lisp
(defparameter grammar nil)
(defparameter lexforms nil)
(defparameter lexicon nil)

(defun which-yalalr ()
  "yalalr, version 1.0")

(defun welcome()
  (format t "~%===================================================")
  (format t "~%Welcome to ~A" (which-yalalr))
  (format t "~%an interface to lalrparser.lisp of Mark Johnson")
  (format t "~%---------------------------------------------------")
  (format t "~%Ready.")
  (format t "~%===================================================~%"))

(defun help ()
  (format t "1. Setf your grammar, lexicon, lexforms in these variables,~%2. and call make-lalrparser.")
  (format t "~%   That will create the LALR parse table.~%3. Then do (target-code '(.. source code ..)) to generate target code for source code online.")
  (format t "~2%If you have lexically analyzed offline, and saved the result in 'tokens' file, then just do (target-code) to generate code.")
  (format t "~%Make sure your lexer returns one Lisp list of tokens.")
  (format t"~2%Yacc-like workflow: write a lexer, make sure it wraps the whole thing in (..), and save it in 'tokens' file and do (target-code) in yalalr")
  (format t "~2%-To make changes in your grammar/lexicon/lexforms, repeat 1-2.")
  (format t "~%-Change *ENDMARKER* from ~A to anything, if you have to." *ENDMARKER*)
  (format t "~% Don't forget to change it in the lexicon as well if you do.")
  (which-yalalr))

;;; Unfortunately, lalrparser.lisp is not publicized as a package so we need to recreate
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

(defun target-code (&optional (words (with-open-file (s "tokens" :direction :input
							:if-does-not-exist :error)
				       (read s))))
  "sticks in the end marker to lexically analyzed words, 
  to pass on to lalrparser's overridden parse function.
  Reads from tokens file by default."
  (parse (append words (list *ENDMARKER*))))
