(defun mysort (lst fun)
  (if lst
    (let* ((head (car lst))
           (tail (cdr lst))
           (left (remove-if (lambda (x) (funcall fun head x)) tail))
           (right (remove-if (lambda (x) (not (funcall fun head x))) tail)))
      (append (mysort left fun) (list head) (mysort right fun)))
  nil))

(princ (mysort '(50 1 3 5 2 4 6 100) #'<))
