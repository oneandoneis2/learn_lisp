; map is for any sequence, needs to be told what type
(map 'list #'* '(1 2 3 4) '(3 3 3 3))
;> (3 6 9 12)
(map 'vector #'* #(1 2 3 4) #(3 3 3 3))
;> #(3 6 9 12)

;map-into - first argument is a sequence that will be used to hold the output
(let ((a (list 1 2 3)))
  (map-into a #'+ '(4 5 6 7) '(40 50 60 70 80))
  (princ a))
;> (44 55 66)
