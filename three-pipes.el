(eval-when-compile (require 'cl))
(defvar 3p-*stack* nil)
(defvar 3p-*stack-ptr* 0)
(defvar 3p-*dicts* (list (make-hash-table)))
(defvar 3p-*context* nil)
(defvar 3p-*compiled-anonymous-words-cache* (make-hash-table :weakness 'key))
(defvar 3p-*stack-depth* 1000)

(defun 3p-get-compiled (key)
  "Fetch a compiled anonymous word from the weak table 3p-*compiled-anonymous-words-cache*."
  (gethash key 3p-*compiled-anonymous-words-cache*))

(defun 3p-add-compiled (key compiled)
  "Add a compiled anonymous word to the weak table 3p-*compiled-anonymous-words-cache*."
  (puthash key compiled 3p-*compiled-anonymous-words-cache*))

(defmacro 3p-example (&rest rest)
  "A form which evaluates to nil, useful for inserting examples
  into the text of the program."  
  nil)

(defmacro 3p-with-fresh-stack (&rest body)
  "Provide an execution context for 3p, giving a fresh stack and
a context variable, used for errors messages."
  `(let ((3p-*stack* (make-vector 3p-*stack-depth* nil))
		 (3p-*stack-ptr* -1)
		 (3p-*context* 'fresh-stack))
	 ,@body))

(defun 3p-push (item)
  "Push an item onto the 3p stack."
  (setq 3p-*stack-ptr* (+ 1 3p-*stack-ptr*))
  (aset 3p-*stack* 3p-*stack-ptr* item)
  item)

(defun 3p-pop ()
  "Pop an item from the 3p stack."
  (if (< 3p-*stack-ptr* 0)
	  (error "3p: stack underflow in context: %s." 3p-*context*)
	(prog1 (aref 3p-*stack* 3p-*stack-ptr*)
	  (aset 3p-*stack* 3p-*stack-ptr* nil)
	  (setq 3p-*stack-ptr* (- 3p-*stack-ptr* 1)))))

(defun 3p-drop ()
  "Drop an item from the 3p stack."
  (3p-pop)
  nil)

(defun 3p-lookup-word (symbol dicts)
  "3p words are stored in a stack of dictionaries.  This function
fetches the most recent definition from the stack of hash tables
DICT, usually 3p-*dicts*."
  (if (null dicts)
	  (error "3p: undefined word: %s (context: %s)" symbol 3p-*context*)
	(let* ((dict (car dicts))
		   (word (gethash symbol dict)))
	  (if word word (3p-lookup-word symbol (cdr dicts))))))

;; A struct representing a 3p word.  Words have a tag (:immediate or
;; :regular) and an implementation, a lambda or no arguments and whose
;; return value is thrown away.
(cl-defstruct 3p-word-rep tag impl)

(defun 3p-extract-glue-word-arity (name)
  "3p provides a concise syntax for calling emacs functions as
words: <arity>symbol.  This function extracts the arity from a
valid symbol name, or returns nil."
  (progn 
	(string-match "<\\([0-9]+\\)>.*" name)
	(let* ((start (match-beginning 1))
		   (end (match-end 1)))
	  (if start 
		  (car (read-from-string (substring name start end)))
		nil))))

(defun 3p-join-str (lst delim)
  "Join the list of strings in LST with the delimeter DELIM."
  (let ((out "")) 
	(loop for (a . rest) on lst do
		  (setq out (concat out a))
		  (if rest (setq out (concat out delim))))
	out))

(defun 3p-extract-glue-word-fun-name (word-name)
  "3p provides a concise syntax for calling emacs functions as
words: <arity>symbol.  This function extracts the function name
from a valid symbol name, or returns nil."
  (progn 
	(string-match "<\\([0-9]+\\)>\\(.*\\)" word-name)
	(let* ((start (match-beginning 2))
		   (end (match-end 2)))
	  (if start (intern (substring word-name start end)) nil))))

(defun 3p-extract-glue-word-fun (word-name)
  "3p provides a concise syntax for calling emacs functions as
words: <arity>symbol.  This function extracts the function
binding associated with the function name from a valid symbol
name, or returns nil."
  (let ((nm (3p-extract-glue-word-fun-name word-name)))
	(if nm (ignore-errors (symbol-function nm)) nm)))

(3p-example 
 (3p-extract-glue-word-arity "<3>concat")
 (3p-extract-glue-word-fun "<3>concat")
 (3p-extract-glue-word-fun-name "<3>concat")
 (read-from-string "3"))

(defun 3p-glue-word-p (symbol)
  "Returns T when symbol denotes a glue word."
  (and (symbolp symbol)
	   (let ((name (symbol-name symbol)))
		 (and (string= "<" (substring name 0 1))
			  (integerp (3p-extract-glue-word-arity name))
			  (3p-extract-glue-word-fun name)))))

(defun 3p-compile-glue-word (symbol)
  "Compile a glue word expression."
  (let* ((name (symbol-name symbol))
		 (arity (3p-extract-glue-word-arity name))
		 (fun (3p-extract-glue-word-fun-name name)))
	`(3p-push (apply #',fun (reverse (list ,@(loop for i from 0 below arity collect `(3p-pop))))))))

(defun 3p-word-p (symbol)
  "Returns T when SYMBOL denotes a 3p-word.  That is, when it denotes a word in the 3p-*dicts*."
   (and (symbolp symbol) (3p-lookup-word symbol 3p-*dicts*)))

(3p-example 
 (3p-compile-glue-word '<3>concat))

(defun 3p-immediate-word-p (3p-word)
  "Returns T when the 3p word is immediate."
  (if 3p-word (equal :immediate (3p-word-rep-tag 3p-word))))

(defun 3p-regular-word-p (3p-word) 
  "Returns T when the 3p word is regular."
  (if 3p-word (equal :regular (3p-word-rep-tag 3p-word))))

(defun 3p-compile-regular-word (3p-word)
  "Calculate the compiled version of the 3p-word."
  `(funcall ,(3p-word-rep-impl 3p-word)))

(defmacro 3p-define-word (name &rest body)
  "Define a 3p word in terms of 3p code and add it to the top-level 3p dictionary."
  `(puthash ',name 
			(make-3p-word-rep :tag :regular 
							  :impl
							  (cadr (macroexpand-all ',`(lambda ()
														 (|||- ,@body)))))
			(car (reverse 3p-*dicts*))))

(defmacro 3p-define-word-in-lisp (name &rest body)
  "Define a 3p word in lisp and add it to the top-level dictionary."
  `(puthash ',name 
			(make-3p-word-rep :tag :regular 
							  :impl
							  (cadr (macroexpand-all ',`(lambda ()
										,@body))))
			(car (reverse 3p-*dicts*))))

(defmacro 3p-define-dyadic-words (&rest names)
  "Given a list of symbols denoting binary elisp functions,
define 3p-words taking exactly 2 arguments."
  `(progn ,@(loop for name in names collect 
				  `(3p-define-word ,name ,(intern (concat "<2>" (symbol-name name)))))))

(defun 3p-execute-immediate-word (word compiled uncompiled)
  "3p immediate words are executed during compilation and recieve
a stack with two arguments: on the top of the stack the list of
uncompiled words ahead of the immediate word, and beneath that a
list representing the compiled elisp for the previous words.
They must leave these two items on the stack, but may modify
them."
  (3p-with-fresh-stack 
   (3p-push compiled)
   (3p-push uncompiled)
   (funcall (3p-word-rep-impl word))
   (let ((uncompiled (3p-pop))
		 (compiled (3p-pop)))
	 (list compiled uncompiled))))

(defmacro |||- (&rest words)
  "Compile the list of forms WORDs, constituting a 3p program, into elisp, and execute it."
  `(progn 
	,@(let ((compiled nil)
			(uncompiled words)) 
		(loop while uncompiled 
			  collect
			  (let ((word (pop uncompiled))) 
				(cond 
				 ((or (stringp word)
					  (keywordp word)
					  (numberp word)
					  (equal word t)
					  (equal word nil))
				  (push `(3p-push ,word) compiled))
				 ((3p-glue-word-p word)
				  (push (3p-compile-glue-word word) compiled))
				 ((3p-word-p word)
				  (let ((3p-word (3p-lookup-word word 3p-*dicts*)))
					(cond 
					 ((3p-immediate-word-p 3p-word)
					  (let ((result (3p-execute-immediate-word 3p-word compiled uncompiled)))
						(setq compiled (car result))
						(setq uncompiled (cadr result))))
					 ((3p-regular-word-p 3p-word)
					  (push (3p-compile-regular-word 3p-word) compiled))
					 (:otherwise (error "Found a 3p word %S but it is neither an immediate nor a regular word. :context %s" word 3p-*context*)))))
				 ((listp word)
				  (cond 
				   ((equal (car word) 'quote)
					(push `(3p-push ,word) compiled))
				   ((equal (car word) (car '`(a b c)))
					(push `(3p-push ,word) compiled))
				   (:otherwise 
					(push `(3p-push (quote ,word)) compiled))))
				 (:otherwise 
				  (error "Can't compile form %s. context: %s (%s %s %s)" 
						 word 
						 3p-*context* 
						 (keyshash (car 3p-*dicts*)) 
						 (3p-lookup-word word 3p-*dicts*)
						 (3p-word-p word))))))
		(reverse compiled))))

(defmacro ||| (&rest words)
  "Compile the forms WORDS, a 3p program, into elisp and execute it with a fresh 3p environment."
  `(3p-with-fresh-stack (|||- ,@words) (if (>= 3p-*stack-ptr* 0) 
										   (3p-pop))))

;;; Define basic dyadic wordsl
(3p-define-dyadic-words 
 + - * / < > <= >= equal eql equalp = concat append cons)

;;; Define swap
(3p-define-word-in-lisp swap 
  (let ((tmp (aref 3p-*stack* 3p-*stack-ptr*)))
	(aset 3p-*stack* 3p-*stack-ptr* (aref 3p-*stack* (- 3p-*stack-ptr* 1)))
	(aset 3p-*stack* (- 3p-*stack-ptr* 1) tmp)))

;;; Define dip.
;;; dip executes the quotation on the top of the stack on the stack resulting from poping the value beneath it.  
;;; the popped value is restored after the word is executed.
(3p-define-word-in-lisp dip 
  (let ((quotation (3p-pop))
        (top (3p-pop)))
	(3p-push quotation)
	(|||- call)
	(3p-push top)))

;;; Call an anonymous 3p quotation.
(3p-define-word-in-lisp call 
  (let* ((aword (3p-pop))
  		 (compiled (3p-get-compiled aword)))
  	(if compiled (funcall compiled)
  	  (let ((compiled (cadr (macroexpand-all `(lambda () (|||- ,@aword))))))
  		(3p-add-compiled aword compiled)
  		(funcall compiled)))))

;;; 3p if : if the third element on the stack is true, execute the
;;; quotation located at the second element on the stack, otherwise
;;; execute the first element.
(3p-define-word-in-lisp if
  (let* ((false-branch (3p-pop)) 
		 (true-branch (3p-pop))
         (condition (3p-pop)))
    (3p-push (if condition true-branch false-branch))
    (|||- call)))

;;; While the quotation on the top of the stack returns t, repeat it.
(3p-define-word-in-lisp loop-while
  (let ((qtn (aref 3p-*stack* 3p-*stack-ptr*))
		(res (progn (|||- call)
					(3p-pop))))
	(while res (3p-push qtn) (|||- call)
		   (setq res (3p-pop)))))

;;; reify the current stack into a list and push it onto the stack.
(3p-define-word-in-lisp current-stack 
  (3p-push (loop for i from 0 to 3p-*stack-ptr* collect (aref 3p-*stack* i))))

;;; duplicate the top of the stack
(3p-define-word-in-lisp dup 
  (3p-push (aref 3p-*stack* 3p-*stack-ptr*)))

;;; drop the top of the stack
(3p-define-word-in-lisp drop 
  (3p-pop))

;;; Emit a message
(3p-define-word message <1>message drop)

(3p-example 
 (3p-compile-glue-word '<3>concat)
 (||| 1 2 3 4 <2>+)
 (||| 1 2 swap)
 (||| (1 2 3 + +) call)
 (||| "cats" " and " "dogs" <3>concat)
 (||| "hello world" message)
 (||| 3 4 > ("true" message) ("false" message) if)
 (||| 4 4 2 '(+) dip current-stack)
 (||| nil 10 dup (swap cons) dip current-stack)
 (||| nil 10 (dup '(swap cons) dip 1 - dup 0 >) loop-while drop)
 (||| 3 dup *)
 (let ((x 10)) (||| `,x 3 +))
 (||| 4 4 =))

(provide 'three-pipes)




