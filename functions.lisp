; Declared via defun
(defun hello-world () (format t "hello world"))
(hello-world)
;> hello world
; NIL

; Can take arguments
(defun hello-you (name) (format t "Hello, ~a!" name))
(hello-you "Fred")
;> Hello, Fred!
;> NIL

; Can have optional params
(defun my-list (a b &optional c d) (list a b c d))
(my-list 1 2 3)
;> (1 2 3 NIL)

; Specify defaults for optional params
(defun my-list2 (a b &optional c (d 1000000)) (list a b c d))
(my-list2 1 2 3)
;> (1 2 3 1000000)

; Shove arbitrary params into a list
(defun foo (a &rest values) (format t "~a ~a" a values))
(foo 1 2 3 4)
;> 1 (2 3 4)

; Or have keyword params
(defun bar (&key a b c) (format t "~a ~a ~a" c b a))
(bar :a 1 :c 2)
;> 2 NIL 1

; ...which can also take defaults
(defun bar2 (&key a (b "woo!") c) (format t "~a ~a ~a" c b a))
(bar2 :a 1 :c 2)
;> 2 woo! 1

; Use #' to get a function reference - sugar for (function)
(defun my-double (x) (* x 2))

(function my-double)
;> #<FUNCTION MY-DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN MY-DOUBLE)) (BLOCK MY-DOUBLE (* X 2))>
#'my-double
;> #<FUNCTION MY-DOUBLE (X) (DECLARE (SYSTEM::IN-DEFUN MY-DOUBLE)) (BLOCK MY-DOUBLE (* X 2))>

; Then use the function for things like mapping:
(mapcar #'my-double '(1 1 2 3 5))
;> (2 2 4 6 10)


; Higher-order functions are done via funcall or apply
; apply takes a list for its params
(defun three-doubles ((a 0) (b 0) (c 0)) (list (* a 2) (* b 2) (* c 2)))
(funcall #'three-doubles 2 4 6)
;> (4 8 12)
(apply #'three-doubles '(2 4 6))
;> (4 8 12)

; Anonymous functions declared via lambda
(map 'list (lambda (x) (+ 3 x)) (list 0 3 6))
;> (3 6 9)
