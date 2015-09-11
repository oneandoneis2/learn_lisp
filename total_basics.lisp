; Lisp: LISt Processor
; (Or maybe Lots of Indenting and Silly Parentheses :)

; Lists are collections of zero or more items, separated by whitespace and grouped by ()s

; Lisp is 'modal' - a bit like vi. A list can be interpreted in either data or command mode.
; In command mode, the first item is considered to be the function name, and everything else is the arguments to the function. In data mode, the items are just items.

; Command mode is the default.

; Right. Let's get started. A Lisp form:
(list 1 2 3)
;> (1 2 3)

; The first item in the list is the function name, 'list'
; This function just returns the items that follow it as a list
; The same outcome can be had from the quote function:
(quote (1 2 3))
;> (1 2 3)

;And quote can be shortened to just '
'(1 2 3)
;> (1 2 3)

; quoting is what puts you into data mode
(+ 2 3)
;> 5
'(+ 2 3)
;> (+ 2 3)

; Because lists are how both data and functions are written, they are pretty much interchangable
(eval '(+ 2 3))
;> 5

; Also, there's quasi-quoting for interpolation:
`(the first item in (1 2 3) is ,(car (list 1 2 3)) ok?)
;> (THE FIRST ITEM IN (1 2 3) IS 1 OK?)

; S-expressions
; Made up of lists and atoms.
; Lists are zero or more elements, separated by whitespace, delimited by parentheses
; Atoms are everything else - numbers, strings, symbols... nil is both atom and list.
; A list can be just a list - data only - or a form - first element is a function
