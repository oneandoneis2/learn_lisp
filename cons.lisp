; Lists are made out of cons cells - which are just pairs of references
; Think of them as [ ][ ]
; So to get 1 and 2 into cons cells, i.e.  1  2
;                                          ↑  ↑
;                                         [ ][ ]
(cons 1 2)
;> (1 . 2)
; the dot-notation is a valid shortcut for writing, too
 (cdr '(1 . 2))
;> 2

; Can cons things together, too:
(cons 1 (cons 2 3))
;> (1 2 . 3)
; A list is just a chain of things cons'd together with a nil at the end
(cons 1 (cons 2 nil))
;> (1 2)

; From this we can get infinite lists just by pointing the last element at the first!
(defvar il (list 1 2))
;                 2  ()
;                 ↑  ↑
;              1 [ ][ ]
;              ↑  ↑
; il is now:  [ ][ ]
(setf (cddr il) il)
;                 2
;                 ↑
;              1 [ ][ ]
;              ↑  ↑  |
; il is now:  [ ][ ]<┘
; This will tend to break things in a REPL unless we tell it to be clever
(setf *print-circle* t)
; That should do it!
il
;> #1=(1 2 . #1#)
(first il)
;> 1
(second il)
;> 2
(third il)
;> 1
(fourth il)
;> 2
; See? Infinite looping list!
