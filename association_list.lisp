; An association list, or alist
(defparameter *foo* '((inquisition . "Nobody expects the spanish inquisition!")
                      (lumberjack . "I'm a lumberjack and I'm okay")
                      (brian . "The babe they call Brian")))
(assoc 'lumberjack *foo*)
;> (LUMBERJACK . "I'm a lumberjack and I'm okay")
(cdr (assoc 'lumberjack *foo*))
;> "I'm a lumberjack and I'm okay"

; A property list, or plist
(defparameter *bar* '(parrot "It's dead, that's what's wrong with it!"
                      :gourd "How much do you want for it?"
                      three "Three shall be the number thou shalt count"))
(getf *bar* 'three)
;> 2
(getf *bar* :gourd)
;> "How much do you want for it?"
(setf (getf *bar* :gourd) "What's wrong with it?")
(getf *bar* :gourd)
;> "What's wrong with it?"
(remf *bar* :gourd)
(getf *bar* :gourd)
;> NIL
; Also worth noting: All symbols have an associated plist, accessed via symbol-plist or get
