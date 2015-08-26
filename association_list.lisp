; An association list, or hash
(defparameter *foo* '((inquisition . "Nobody expects the spanish inquisition!")
                      (lumberjack . "I'm a lumberjack and I'm okay")
                      (brian . "The babe they call Brian")))
(assoc 'lumberjack *foo*)
;> (LUMBERJACK . "I'm a lumberjack and I'm okay")
(cdr (assoc 'lumberjack *foo*))
;> "I'm a lumberjack and I'm okay"
