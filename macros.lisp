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
