; So let's think about what we want to do with lists, typically
; rearrange, map, filter, combine, and consume (fold) seem to cover the main operations

; rearrange = change the ordering of a list.
; Alphabetical order, numeric order, etc.
; sort will probably do us: Takes a list and a function to sort by
(sort '(1 3 2 4) #'>)
;>(4 3 2 1)
(sort '(1 3 2 4) #'<)
;>(1 2 3 4)
; Also, things like reverse can be handy
(reverse '(1 2 3 4))
;>(4 3 2 1)

; map = generate a new list that's a transformation of an old list
; Typically, takes a function to do the mutation, followed by the sequence to mutate
; Sadly, there's multiple types of map:
; map, mapc, mapcan, mapcar, mapcon...
; map works on any sequence - but you have to tell it what it's handling
(map 'list (lambda (x) (+ x 1)) '(1 2 3 4))
;> (2 3 4 5)
(map 'string (lambda (x) (char-upcase x)) "a B c")
;> "A B C"
; mapcar is just for lists - nice & simple
(mapcar (lambda (x) (* 2 x)) '(1 2 3 4))
;>(2 4 6 8)
; maplist passes the actual cons cells, not the values
(mapcar (lambda (x) (format t "~a~%" x)) '(1 2))
;> 1
;> 2
(maplist (lambda (x) (format t "~a~%" x)) '(1 2))
;> (1 2)
;> (2)
; mapcan/mapcon



;filter
(remove-if (lambda (x) (eq 1 (mod x 2))) '(1 2 3 4 5 6))
;> (2 4 6)

; combine lists
(append '(1 2) '(3 4))
;>(1 2 3 4)

; consume/reduce/fold lists - take a list and just return a single value
(reduce #'+ '(1 2 3 4))
;> 10

; Simple addition and subtraction of values
(defparameter foo '(1 2 3))
(push 0 foo)
;> (0 1 2 3)
(pop foo)
;> 0

; List processing functions are usually functional:
; They don't modify the original list
(defparameter foo (list 1 2 3 4))
(defparameter bar (cdr foo))
(setf foo (reverse foo))
bar
;> (2 3 4)
; But non-consing variants exist, which means they don't allocate new cons cells
; i.e. they are destructive & use existing ones. eg:
(defparameter foo (list 1 2 3 4))
(defparameter bar (cdr foo))
(setf foo (nreverse foo))
bar
;> (2 1)

; And then there's destructuring bind
; This is a macro that lets you split up a list like a function/macro's params
; Three parts: The param list, the arguments list, and the body
; Kinda an annoying way of doing pattern matching
(destructuring-bind
  (x y)     ; param list - just like a function's
  '(1 2)    ; Arguments list - just like when calling a function
  (format t "~a ~a" x y))   ; Body: What to do with the args you bound to params
;> 1 2
(destructuring-bind
  (x (y1 y2) z)
  '(1 (2 3) 4)
  (format t "~a ~a ~a ~a" x y1 y2 z))
;> 1 2 3 4
(destructuring-bind
  (x (y1 &optional y2) z)   ; Can have optional params
  '(1 (2 3) 4)
  (format t "~a ~a ~a ~a" x y1 y2 z))
;> 1 2 3 4
(destructuring-bind
  (x (y1 &optional y2) z)
  '(1 (2) 4)
  (format t "~a ~a ~a ~a" x y1 y2 z))
; 1 2 NIL 4
(destructuring-bind
  (x (y1 &optional (y2 'erk)) z)    ; Can specify defaults
  '(1 (2) 4)
  (format t "~a ~a ~a ~a" x y1 y2 z))
;> 1 2 ERK 4
(destructuring-bind
  (x &rest y)    ; Bung everything else into a list
  '(1 2 3 4)
  (format t "~a ~a" x y))
;> 1 (2 3 4)
(destructuring-bind
  (&key x y z)    ; Order by keyword
  '(:z 1 :y 2 :x 3)
  (format t "~a ~a ~a" x y z))
;> 3 2 1
(destructuring-bind
  (&whole all x y z)    ; Keep original list whilst also breaking into params..
  '(1 2 3)
  (format t "~a ~a ~a ~a" x y z all))
;> 1 2 3 (1 2 3)
; And so on and so forth - just like with functions!

; Other listy things
(nth 2 '(1 2 3 4))
;> 3

(list* 1 2)
;> (1 . 2)
(list* 1 2 3)
;> (1 2 . 3)
