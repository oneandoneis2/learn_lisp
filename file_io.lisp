; Can easily save data to file and load it back
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
