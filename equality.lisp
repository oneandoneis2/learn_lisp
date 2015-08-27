; Somewhat excessively, we don't just have ==
; We have eq, eql, equal and equalp!
; They are steadily more discerning:
; EQ: Basically, if they're not the same symbol, no
; EQL: Also allows same-value objects, such as characters but not strings
; EQUAL: Also allows same-value lists and strings
; EQUALP: Even allows strings with different case and floats compared to ints!
; LoL suggests using EQ for symbols and EQUAL for everything else. Seems sane to me!

; These are all false
(eq "abc" "abc")
(let ((foo "abc") (bar "abc")) (eq foo bar))
(eql "abc" "abc")
(eql "a" "a")
(eql '(1 2 3) '(1 2 3))
(equal 1 1.0)

; These are all true
(eq () nil)
(eq 'abc 'abc)
(eq 1 1)
(let* ((foo "abc") (bar foo)) (eq foo bar))
(eql #\a #\a)
(equal "abc" "abc")
(equal '(1 2 3) '(1 2 3))
(equalp "abc" "aBc")
(equalp 1 1.0)
