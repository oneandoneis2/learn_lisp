; Sometimes you want to get something out of a list
; Instead of a nice helpful "head" and "tail" convention, there's "car" and "cdr"

(defvar foo '(1 2 3 4 5 6))
(car foo)
;> 1
(cdr foo)
;> (2 3 4 5 6)

; The nice thing is, you can combine them
(cddr foo)
;> (3 4 5 6)
(cdddr foo)
;> (4 5 6)
(cadr foo)
;> 2
(caddr foo)
;> 3

; Which is also helpful for multi-dimensional lists
(defvar bar `((1 2 3) (a b c)))
bar
;> ((1 2 3) (A B C))
(car bar)
;> (1 2 3)
(cdr bar)
;> ((A B C))
(cadr bar)
;> (A B C)
(cadadr bar)
;> B
(caar bar)
;> 1

; For more readable occasions, there are alternatives:
(first foo)
;> 1
(second foo)
;> 2
(third foo)
;> 3
(fourth foo)
;> 4
(fifth foo)
;> 5
