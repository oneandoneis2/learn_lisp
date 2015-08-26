; The basic standards: if, if-else, when, unless, and two switch's

; If:
(if '(1)
  (princ "yes"))
;> "yes"
(if ()
  (princ "yes"))
;>

; If-else:
(if '(1)
  (princ "yes")
  (princ "no"))
;> "yes"
(if ()
  (princ "yes")
  (princ "no"))
;> "no"

; When:
(when '(1)
  (print "I do one thing")
  (print "Then another!")
  (print "'if' can only handle one thing")
  (print "Unless you use progn, that is"))
;> "I do one thing"
;> "Then another!"
;> "'if' can only handle one thing"
;> "Unless you use progn, that is"

; Unless is just a negated 'when'
; Then there's cond - the first switch
(cond ((> 2 x) (princ "First"))
      ((> 5 x) (princ "Second"))
      (t (princ "Default")))
; and case, the other switch
(case x
  ((1) (princ "First"))
  ((3) (princ "Second"))
  (otherwise (princ "Default")))
; If x = 1
;> "First"
; if x = 3
;> "Second"
; if x = 10
;> "Default"
; There's no fall-through - you get the first match and then you're done.
; Which IMHO is a far more sensible approach
; Use case when always looking at the same var, cond for everything else

; There are also the "stealth" conditionals, which are helpful if you need short-circuiting
; "and" and "or"!
; (note that 'if' also short-circuits, though)
(and (print "one") (print "two"))
;> "one"
;> "two"
(and (print "one") () (print "two"))
;> "one"
(or (print "one") (print "two"))
;> "one"
(or () (print "two"))
;> "two"
; To prove short-circuiting, note that this doesn't blow up:
(or (print "1") (/ 0 0))
;> "1"
