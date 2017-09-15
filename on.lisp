((lambda (x) (* x 2)) 3)
; 6
(defun double (x) (* x 2))
#'double
; #<FUNCTION DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN DOUBLE)) (BLOCK DOUBLE (* X 2))>
#'(lambda (x) (* x 2))
; #<FUNCTION :LAMBDA (X) (* X 2)>
(symbol-function 'double)
; #<FUNCTION DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN DOUBLE)) (BLOCK DOUBLE (* X 2))>
; defun builds the function; then associates it with the symbol. Equivalent to
(setf (symbol-function 'double) #'(lambda (x) (* x 2)))

(+ 1 2 3)
; equivalent to
(apply #'+ (list 1 2 3))
; equivalent to
(apply #'+ 1 2 (list 3))
; equivalent to
(funcall #'+ 1 2 3)

(sort '(1 9 3 6 5 2) #'<)
; (1 2 3 5 6 9)
(sort '(1 9 3 6 5 2) #'>)
; (9 6 5 3 2 1)

(let ((y 7))
  (defun scope-test (x) (list x y)))
(let ((y 5)) (scope-test 3))
; (3 5) in dynamic Lisp
(let ((y 5)) (scope-test 3))
; (3 7) in lexical Lisp

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))
; Closures!

(defun plus_one (x) (+ 1 x))
(compiled-function-p #'plus_one)
; NIL
(compile 'plus_one)
(compiled-function-p #'plus_one)
; T
; Function compilation!
(defun make-adder (x)
  (lambda (y) (+ x y)))
(compile 'make-adder)
(let ((add-five (make-adder 5)))
  (compiled-function-p add-five))
; T
; Compiled function generation!
(defun 50th (lst) (nth 49 lst))
(proclaim '(inline 50th))
(defun foo (lst) (+ (50th lst) 1))
; Above is equivalent to (defun foo (lst) (+ (nth 49 lst) 1))

; do syntax
(do ((i 0 (+ 1 i)))     ; Declare vars & their incremental values
  ((>= i 10))           ; State end condition - checked before executing body
  (format t "~a~%" i))  ; Loop body

; Returning/catching multiple return values:
(multiple-value-bind
  (foo bar)                     ; vars to be created & assigned to
  (values 3 5)                  ; return two values, 3 and 5
  (format t "~a ~a" bar foo))   ; body where created vars have been assigned
; 5 3

; Beware modifying/returning quoted lists
(+ 1 2)
; 3
*
; 3
(defun exclaim (expr) (append expr '(oh my)))
(exclaim '(lions and tigers and bears))
; (LIONS AND TIGERS AND BEARS OH MY)
(nconc * '(goodness))
; (LIONS AND TIGERS AND BEARS OH MY GOODNESS)
(exclaim '(foo and bar and baz))
; (FOO AND BAR AND BAZ OH MY GOODNESS)
; The nconc has modified the list used by exclaim :(
; To avoid, use (list) not '() - see:
(defun exclaim2 (expr) (append expr (list 'oh 'my)))
(exclaim2 '(lions and tigers and bears))
; (LIONS AND TIGERS AND BEARS OH MY)
(nconc * '(goodness))
; (LIONS AND TIGERS AND BEARS OH MY GOODNESS)
(exclaim2 '(foo and bar and baz))
; (FOO AND BAR AND BAZ OH MY)


