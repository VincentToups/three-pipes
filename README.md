||| Three Pipes |||
===================

Three pipes is a stack language built to run inside of and in accord
with Emacs Lisp.

Three pipes resembles Forth or Factor, (more the latter) in that it is
a concatenative language where juxtaposition represents COMPOSITION of
functions.  That is the academic explanation.  The Joy of
concatenative languages is their great terseness and, regularity, and
expressiveness.  Stack languages are more Lispy than Lisp!  Now all
that power is right in your Emacs Lisp code:

    (||| 4 4 +) ; -> 8

    (||| "Hello World!" 10 
        ((dup <1>insert) dip 1 - dup 0 >) loop-while)
    ;; prints "Hello World" ten times

Usage
=====

Put three-pipes.el on your load-path and then `(require
'three-pipes)`.

Basic Syntax
============

The entry point for Three Pipes is three pipes:`|||`.  The body of the
three pipes macro is treated as a list of three pipes words.  Atoms
simply denote words which push themselves onto the stack.  Regular
symbols denote a three pipes word, a function which works a side
effect on the stack, and special symbols of the form
`<integer>symbol-name`, denote "glue words" which tell the compiler
that you want to call the Emacs Lisp function `symbol-name` with
`integer` arguments from the stack, and have the result pushed onto
the stack.

For instance:

    (||| "Raining " "cats " "and " "dogs." <4>concat)
    ;;; -> "Raining cats and dogs."

Lists are read as _quotations_, delayed three pipes programs which can
be executed with the three pipes word `call`:

    (||| 3 (+ 2) call) ;;; -> 5

One may optionally quote such lists:

    (||| 3 '(+ 2) call) ;;; -> 5

Backquote can be used to splice values from the environment into the
three-pipes code:

    (let ((x 10)) (||| `,x 3 +)) ;;; -> 13

And hence to side effect values in the environment as well.

Words
=====

Three pipes supports a small vocabulary of built in words:

    dup dip swap if loop-while drop + - * / = equal eql concat append
    cons

And more words can be defined with the macros 

    3p-define-word 
    3p-define-word-in-lisp

The former allows you to define new words in terms of three pipes
programs while the latter allows you to use the primitive lisp
functions `3p-push` and `3p-pop` as well as `|||-` which executes a
fragment of three pipes code without resetting the context, to define
new words in elisp.  I plan on adding lots of words soon.


