; Before we get to loop itself, some alternatives
; dolist - iterates over a list
(dolist (x (list 1 2 3)) (print x))
;> 1
;> 2
;> 3
(dolist (x (list 1 2 3)) (print x)(if (evenp x)(return)))
;> 1
;> 2
; dotimes - iterate a given number of times
(dotimes (i 3) (print i))
;> 0
;> 1
;> 2
; Both are based on do, which is very powerful and unintuitive
; (do (variable definitions: name, initial value, step)
;     (end-test-form results)
;     statements)   ; often not needed due to power of other parts!
(do ((i 1 (* 2 i))  ; i starts as one and gets doubled
     (j 8))         ; j starts as 8, nothing happens to it
  ((eql i j) (format nil "i and j are both ~a" i)) ; return string when end condition met
  (print i))        ; Each iteration, print current value of i
;> 1
;> 2
;> 4
;> "i and j are both 8"

; And then there is loop...
; Generate a list of 1-10
(do ((nums nil) (i 1 (incf i)))
  ((> i 10) (reverse nums))
  (push i nums))
; or alternatively
(loop for i from 1 to 10 collecting i)
