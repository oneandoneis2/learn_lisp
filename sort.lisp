(defun mysort (lst fun)
  (if lst
    (let* ((head (car lst))
           (tail (cdr lst))
           (less (remove-if (lambda (x) (not (funcall fun x head))) tail))
           (great (remove-if (lambda (x) (funcall fun x head)) tail)))
      (append (mysort less fun) (list head) (mysort great fun)))
  nil))

(princ (mysort '(50 1 3 5 2 4 6 100) #'<))
