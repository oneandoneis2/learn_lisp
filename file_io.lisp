; Line 1
; Line 2
(quote (1 2 3))
(quote (1 2 3))

; Open files with "open", close them with "close" - stop me if this is too technical
(open "file_io.lisp")
; Read lines from text file with read-line
(format t "~a" (read-line (open "file_io.lisp")))
;> ; Line 1
; Read characters one-by-one with read-char
(format t "~a" (read-char (open "file_io.lisp")))
;> ;
; Stick it into a var to actually use the stream properly
(let ((file (open "file_io.lisp")))
  (format t "~a" (read-line file))
  (format t "~a" (read-line file))
  (close file))
;> ; Line 1; Line 2

; read-line and read-char do exactly what you'd expect
; read is a little different: It reads in s-expressions
(let ((file (open "file_io.lisp")))
  (format t "~a~%" (read-line file))
  (format t "~a~%" (read-line file))
  (format t "~a~%" (read-line file))
  (format t "~a~%" (read file))
  (close file))
;> ; Line 1
;> ; Line 2
;> ; (quote (1 2 3))
;> ; '(1 2 3)
; First s-exp is treated as a string. Second is interpreted, hence quote becomes '
; Using read with print, can easily save data to file and load it back
(defparameter stuff '((:a 1)(:b 2)))
(with-open-file (out "file_io.txt"
                     :direction :output
                     :if-exists :supersede)
  (with-standard-io-syntax (print stuff out)))
; In txt file:
;> ((:|A| 1.) (:|B| 2.))

(defparameter newstuff (with-open-file (in "file_io.txt")
                         (with-standard-io-syntax (read in))))
newstuff
;> ((:A 1) (:B 2))

; Binary data is read in similar ways, only using read-byte
; There are write- counterparts to the read- functions

; Use with-open-file instead of open-then-close
