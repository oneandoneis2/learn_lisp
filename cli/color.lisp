; Some pretty-print options
(defun printc (col str)
  (let ((cols '((red . 31)
                (green . 32)
                (yellow . 33)
                (blue . 34)
                (purple . 35)
                (cyan . 36))))
    (labels ((getcode (col) (or (cdr (assoc col cols)) 0)))
      (format nil "~c[~am~a~c[0m" #\ESC (getcode col) str #\ESC))))

(defun test-equality (v1 v2)
  (mapcar (lambda (tst) (eqtst tst v1 v2)) '(eq eql equal)))

(defmacro eqtst (test v1 v2)
;  `(format t "~a ~a ~a~%" ,test ,v1 ,v2))
  `(if (,test ,v1 ,v2)
     (format t "~a ~a~%" ,test (printc 'green "/"))))
