(defparameter *something-global* nil)
; This defines a global variable
; The asterisks are a convention, not a necessity.
; There's no hoisting of declarations: Variables exist only after they are defined:

(defparameter *something-global-2* nil)
(princ *something-global-2*)
; ^ Valid

(princ *something-global-3*)
(defparameter *something-global-3* nil)
(princ *something-global-3*)
; ^ Crash and burn

; defparameter always overwrites the value of a variable
; Alternatively, you can use defvar - declares a global variable, but if re-declared, doesn't change the value
(defparameter *something-global-4* nil)
(defparameter *something-global-4* 1)
(princ *something-global-4*)
;> 1

(defvar *something-global-5* nil)
(defvar *something-global-5* 1)
(princ *something-global-5*)
;> nil

; Can also just use a 'raw' setf to declare a variable
; set and setq are obsolete, do not use
(setf foo (+ 2 3))
;> 5
(* 2 foo)
;> 10
; There's also handy shortcuts for things like foo++
(format t "~a ~a ~a" foo (incf foo) foo (decf foo))
;> 5 6 6 5
; You can swap two values with rotatef
(let ((a 1)(b 2)) (rotatef a b)(format t "~a ~a" a b))
;> 2 1
; And shuffle values along in a chain with shiftf
(let ((a 1)(b 2)) (shiftf a b 3)(format t "~a ~a" a b))
;> 2 3

; And then there's lexical scoping
(let ((x 5) (y 6))
  (format nil "~a ~a" x y))
;> "5 6"

(let ((x 5) (y 6))
  (let ((x 10))
    (format nil "~a ~a" x y)))
;> "10 6"
;The inner x over-rides the outer, as you'd hope

(let ((x 5) (y 6))
  (format t "~a ~a~%" x y)
  (setf x 6)
  (format t "~a ~a~%" x y))
;> 5 6
;> 6 6
; Set the value of a var using setf

(defparameter *close*
  (let ((count 0))
    #'(lambda () (incf count))))
(funcall *close*)
;> 1
(funcall *close*)
;> 2
(funcall *close*)
;> 3
; Yep, closures work as expected! *close* refers to a function with a lexically-scoped variable

; If lexically-scoped vars need to refer to each other, use let*
(let* ((x 5) (y (* x 2)))
  (format nil "~a ~a" x y))
;> "5 10"

; let can also be used to over-ride globals temporarily
(let ((*standard-output* *some-other-stream*))
    (do-stuff))
; Everything done by do-stuff will output to some other stream;
; stdout will then revert to normal outside the scope of the let

; setf works on more than just simple variables - like arrays and hashes - in a seemingly-weird way
; that's actually the same as other languages when you think about it
(let ((h (make-hash-table)))
  (setf (gethash 'foo h) "Wibble")
  (format nil "Foo contains ~a" (gethash 'foo h)))
;> "Foo contains Wibble"
; Although "Use the get function to set it" seems weird, it's basically exactly the same as `$hash{'foo'} = 'bar'
; The same expression that gets the value is used to set it - setf can turn gethash etc. into an L-value`
