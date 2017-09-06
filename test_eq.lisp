(defun test-equalities (a b)
  (labels ((test-equality (tst)
                          (let ((result (ignore-errors (funcall tst a b))))
                            (if result
                              (format t "~c[32m✓~c[0m ~a ~a ~a~%" #\ESC #\ESC a tst b)
                              (format t " ~c[31mx~c[0m ~a ~a ~a~%" #\ESC #\ESC a tst b)))))
    (mapcar #'test-equality '(= eq eql equal equalp string=))
    '---))
