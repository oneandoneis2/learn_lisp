; Macros are essentially a powerful way of writing new syntax
; Let's say that we have an idiom that keeps cropping up: We have many cases
; where we want to return a value if it is true
(defun ri (arg)
  (if arg
    (return-from ri arg)
    0))
; If ri's arg is true, we just return it. Otherwise we return a 0
; We don't want to keep doing "if foo return foo"
; So let's macro it!
(defmacro return-if (f x)
  `(if ,x (return-from ,f ,x)))
; We can now define the function as:
(defun ri (arg) (return-if ri arg) 0)

; Begin to use that whole "code as data" thing - duplicate a form into a string
(defmacro dup_form (form)
  `(format t "~a ~:[is not~;is~] true." ',form ,form))
(dup_form (eq 'foo 'foo))
;> (EQ 'FOO 'FOO) is true.
(dup_form (eq 'foo 'bar))
;> (EQ 'FOO 'BAR) is not true.

; There's also ,@ for when you have an expression that will return a list of expressions:
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(dup_form ,f))))
; The loop will generate a collection of dup_form calls that the progn will then run:
(check (eq 'foo 'foo) (eq 'foo 'bar))
;-> (PROGN (DUP_FORM (EQ 'FOO 'FOO)) (DUP_FORM (EQ 'FOO 'BAR))) ;
;> (EQ 'FOO 'FOO) is true.(EQ 'FOO 'BAR) is not true.

(defmacro test_assert (arg test)
  `(format t "~:[NOT ~;~]OK: Variable ~a = ~a~%" (equal ,arg ,test) (quote ,arg) ,arg))

(let ((foo 10)
      (bar 10))
  (test_assert foo 10)
  (test_assert bar 5))
; OK: Variable FOO = 10
; NOT OK: Variable BAR = 10

; Macros destructure their argument list
(defmacro accept-list-and-arg ((foo bar) baz)
  (format t "~a - ~a - ~a" foo bar baz))
;-> (accept-list-and-arg (1 2) 3)
;> 1 - 2 - 3

; Avoiding naming clashes with gensym
(defmacro bad-name (foo)
  `(let ((bar 10))
     (> bar ,foo)))
; Silly macro returns true if its argument is less than 10

;-> (let ((x 1)) (bad-name x))
;> T
; All good. Let's just check...
;-> (macroexpand-1 `(bad-name x))
;> (LET ((BAR 10)) (> BAR X)) ;

; BUT
;-> (let ((bar 1)) (bad-name bar))
;> NIL
; Why?
;-> (macroexpand-1 `(bad-name bar))
;> (LET ((BAR 10)) (> BAR BAR)) ;
; We've declared a variable with the same name as a variable we're passing in.
; FAIL!

(defmacro good-name (foo)
  (let ((internal-name (gensym)))
    `(let ((,internal-name 10))
       (> ,internal-name ,foo))))

; Gensym guarantees no name clash
;-> (macroexpand-1 `(good-name x))
;> (LET ((#:G3340 10)) (> #:G3340 X)) ;

; Let's write a macro for use in writing macros - something to spare us having to declare all those gensysms
(defmacro with-gensym ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro good-name (foo)
  (with-gensym (internal-name)
    `(let ((,internal-name 10))
       (> ,internal-name ,foo))))
