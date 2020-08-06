(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (&body forms)
  `(progn
     ,@(loop for form in forms collect `(report-result ,form ',form))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5)))

(test-+)
