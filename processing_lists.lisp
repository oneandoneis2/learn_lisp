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
