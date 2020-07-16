; Store all CD records in a "database"
(defvar *db* nil)

; Store CD data in a plist
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; Abstract the push macro for adding CDs to the DB
(defun add-record (cd) (push cd *db*))

; Pretty-print the contents of the DB
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

; Prompt for input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? ")) (return))))

; Save and reload the DB
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

; Add selector
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

; and updater
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title    (setf (getf row :title)  title))
                (if artist   (setf (getf row :artist) artist))
                (if rating   (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

; and deleter
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

; Now macros and helper functions to make code more DRY
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
