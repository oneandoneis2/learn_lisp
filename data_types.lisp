; Starting simply, integers:
1
;> 1
; Rational numbers
(/ 1 2)
;> 1/2
; floats
1.0
; doubles
1.0d0

; Strings
"foo"

; Symbols
'foo
;> FOO
'(foo |foo| \foo)
;> (FOO |foo| |fOO|)
(eq 'foo '|foo|)
;> NIL
(eq 'foo '|FOO|)
;> T

; Symbols don't have to be displayed in uppercase:
(let ((*print-case* :downcase)) (print 'foo))
;> foo
(let ((*print-case* :capitalize)) (print 'foo))
;> Foo
(let ((*print-case* :upcase)) (print 'foo))
;> FOO
; Upcase just happens to be the default



; Keywords
:foo
;> :FOO


