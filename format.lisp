; First param determines whether you print to stdout and return nil
; or return the string
(format t "Hello, world!")
;> Hello, world!
;> NIL

(format nil "Hello, world!")
;> Hello, world!

; Interpolate a variable into text via ~a
(let ((foo "Tim")) (format nil "There are those who call me ~a" foo))
;> "There are those who call me Tim"

; Newline via ~%
(let ((foo "Tim")) (format t "There are those who call me~%~a" foo))
;> There are those who call me
;> Tim
;> NIL

(let ((foo "Tim")
      (bar "An enchanter"))
  (format t "~a~a~%~a~a" :name foo :job bar))
;> NAMETim
;> JOBAn enchanter
; Not so good.. align things a bit more nicely!
(let ((foo "Tim")
      (bar "An enchanter"))
  (format t "~5a~a~%~5a~a" :name foo :job bar))
;> NAME Tim
;> JOB  An enchanter
; The first two vars are expanded to take up 5 chars now.
; Alternatively, tabulate:
(let ((foo "Tim")
      (bar "An enchanter"))
  (format t "~a~5t~a~%~a~5t~a" :name foo :job bar))
;> NAME Tim
;> JOB  An enchanter
; Tabulation says "Move to the nth column" - more reliable

; Iterate over a list with ~{ ~}
(format t "~{~a~}" (list 1 2 3))
;> 123
(format t "~{~a~%~}" (list 1 2 3))
;> 1
;> 2
;> 3

; When more than one argument is expected, it'll consume more of the list
(format t "~{~a ~a~%~}" (list 1 2 3 4 5 6))
;> 1 2
;> 3 4
;> 5 6

; This is handy for dumping plists
(format t "~{~a: ~6t~a~%~}" (list :name "Tim" :job "Enchanter"))
;> NAME: Tim
;> JOB:  Enchanter

; It can even handle nesting!
(format t "~{~{~a:~6t~a~%~}---~%~}" (list (list :name "Tim" :job "Enchanter")
                                          (list :name "Arthur" :job "King")))
;> NAME: Tim
;> JOB:  Enchanter
;> ---
;> NAME: Arthur
;> JOB:  King
;> ---

; Using format to prompt for a name for "Hello, world"
(defun prompt-read (prompt)
  (format *query-io* "~a? " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
(format t "Hello, ~a" (prompt-read "Name"))
;> Name?
;< Dominic
;> Hello, Dominic
