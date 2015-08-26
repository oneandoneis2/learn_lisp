(defun mysort (lst)
  (if lst
    (let* ((head (car lst))
           (tail (cdr lst))
           (less (remove-if (lambda (x) (or (> x head) (eq head x))) tail))
           (great (remove-if (lambda (x) (< x head)) tail)))
      (append (mysort less) (list head) (mysort great)))
  nil))

(princ (mysort '(50 1 3 5 2 4 6 100)))
