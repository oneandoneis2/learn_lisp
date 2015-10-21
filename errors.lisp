; Errors are handled by the condition system. (It has other uses than exceptions)
; Unlike try-catch style systems that are two part: throw error, handle error
; It has three parts: Signal error, handle error, restart

; First, define the condition as an error:
(define-condition   num-is-even (error)
;^condition object  ^name       ^type
  ((num
     :initarg :num  ; <- has one slot, num, with accessor 'num' that's populated
     :reader num))) ;    by supplying a named :num param when calling

; Then write code that calls it
(labels ((test (x) (if (oddp x)
                     x
                     (error 'num-is-even :num x))))
  (loop for i from 5 to 8
        for answer = (test i)
        do (format t "~a~%" answer)))
;> 5
;> *** - Condition of type NUM-IS-EVEN.
; ^ into the debugger!

; To automate handling, add a handler-case
(labels ((test (x) (if (oddp x)
                     x
                     (error 'num-is-even :num x))))
  (loop for i from 5 to 8
        for answer = (handler-case (test i) (num-is-even () (- i 1)))
        do (format t "~a~%" answer)))
;> 5
;> 5
;> 7
;> 7

; So far, so good - this is basically into try-catch levels of power
; The code signals an error, and the handler deals with it
; More usefully, use handler-bind & restarts, rather than handler-case
(define-condition num-is-odd  (error) ((num :initarg :num :reader num)))
(define-condition num-is-big  (error) ((num :initarg :num :reader num)))
(define-condition num-is-zero (error) ((num :initarg :num :reader num)))

; Step 1: Define multiple error handlers
(labels ((test (x) (cond
                     ((= 0 x) (error 'num-is-zero :num x))
                     ((< 4 x) (error 'num-is-big :num x))
                     ((oddp x) (error 'num-is-odd :num x))
                     ((evenp x) (error 'num-is-even :num x)))))
  (loop for i from 0 to 6
        for answer = (handler-case (test i)
                       (num-is-zero (x) (format nil "zero ~a" (num x)))
                       (num-is-odd  (x) (format nil "odd ~a" (num x)))
                       (num-is-big  (x) (format nil "big ~a" (num x)))
                       (num-is-even (x) (format nil "even ~a" (num x))))
                       ; Passing x   ^ gives you the error object if you want it
                       ; Here, I'm using it to get the value of the bad number
        do (format t "~a~%" answer)))
;> zero 0
;> odd 1
;> even 2
;> odd 3
;> even 4
;> big 5

; Step 2: Define restarts instead of error handlers
; This doesn't do much, just means you have choices when you hit the error
(labels ((test (x) (cond
                     ((= 0 x) (error 'num-is-zero :num x))
                     ((< 4 x) (error 'num-is-big :num x))
                     ((oddp x) (error 'num-is-odd :num x))
                     ((evenp x) (error 'num-is-even :num x)))))
  (loop for i from 0 to 6
        for answer = (restart-case (test i)
                       (report-zero () "It's zero")
                       (report-odd  () "It's odd")
                       (report-big  () "It's big")
                       (report-even () "It's even"))
        do (format t "~a~%" answer)))
;> *** - Condition of type NUM-IS-ZERO.
;> The following restarts are available:
;> REPORT-ZERO    :R1      REPORT-ZERO
;> REPORT-ODD     :R2      REPORT-ODD
;> REPORT-BIG     :R3      REPORT-BIG
;> REPORT-EVEN    :R4      REPORT-EVEN
;> ABORT          :R5      Abort main loop

; Step 3: Define handlers for the errors
; This allows us to specify what restart case is called by which error
; In this case:
;   test specifies which errors to throw when
;   the loop specifies what restart cases can be used in the event of an error
;   handler-bind maps the restart cases to errors
(handler-bind ((num-is-big #'(lambda (x) (invoke-restart 'report-big)))
               (num-is-even #'(lambda (x) (invoke-restart 'report-even)))
               (num-is-zero #'(lambda (x) (invoke-restart 'report-zero)))
               (num-is-odd #'(lambda (x) (invoke-restart 'report-odd))))
  (labels ((test (x) (cond
                       ((= 0 x) (error 'num-is-zero :num x))
                       ((< 4 x) (error 'num-is-big :num x))
                       ((oddp x) (error 'num-is-odd :num x))
                       ((evenp x) (error 'num-is-even :num x)))))
    (loop for i from 0 to 6
          for answer = (restart-case (test i)
                         (report-zero () "It's zero")
                         (report-odd  () "It's odd")
                         (report-big  () "It's big")
                         (report-even () "It's even"))
          do (format t "~a~%" answer))))
;> It's zero
;> It's odd
;> It's even
;> It's odd
;> It's even
;> It's big
;> It's big

; Step 4: Refactor so the restarts are as low-level as possible
(handler-bind ((num-is-big #'(lambda (x) (invoke-restart 'report-big)))
               (num-is-even #'(lambda (x) (invoke-restart 'report-even)))
               (num-is-zero #'(lambda (x) (invoke-restart 'report-zero)))
               (num-is-odd #'(lambda (x) (invoke-restart 'report-odd))))
  (labels ((test (x) (restart-case (cond
                                     ((= 0 x) (error 'num-is-zero :num x))
                                     ((< 4 x) (error 'num-is-big :num x))
                                     ((oddp x) (error 'num-is-odd :num x))
                                     ((evenp x) (error 'num-is-even :num x)))
                       (report-zero () "It's zero")
                       (report-odd  () "It's odd")
                       (report-big  () "It's big")
                       (report-even () "It's even"))))
    (loop for i from 0 to 6
          for answer = (test i)
          do (format t "~a~%" answer))))
;> It's zero
;> It's odd
;> It's even
;> It's odd
;> It's even
;> It's big
;> It's big

; Step 5: Add another level of handler
(handler-bind ((num-is-big #'(lambda (x) (invoke-restart 'report-big)))
               (num-is-even #'(lambda (x) (invoke-restart 'report-even)))
               (num-is-zero #'(lambda (x) (invoke-restart 'report-zero)))
               (num-is-odd #'(lambda (x) (invoke-restart 'report-odd))))
  (labels ((test (x) (restart-case (cond
                                     ((= 0 x) (error 'num-is-zero :num x))
                                     ((< 4 x) (error 'num-is-big :num x))
                                     ((oddp x) (error 'num-is-odd :num x))
                                     ((evenp x) (error 'num-is-even :num x)))
                       (retry-bigger () (test (incf x)))
                       (ident () x)
                       (report-zero () "It's zero")
                       (report-odd  () "It's odd")
                       (report-big  () "It's big")
                       (report-even () "It's even"))))
    (loop for i from 0 to 6
          for answer = (handler-bind ((num-is-zero #'(lambda (x) (invoke-restart 'retry-bigger)))
                                      (num-is-big #'(lambda (x) (invoke-restart 'ident))))
                         (test i))
          do (format t "~a~%" answer))))
;> It's odd         due to inner bind
;> It's odd         due to outer bind
;> It's even        due to outer bind
;> It's odd         due to outer bind
;> It's even        due to outer bind
;> 5                due to inner bind
;> 6                due to inner bind

; Step 6: Different handlers specified for different function calls
(labels ((test (x) (restart-case (cond
                                   ((oddp x) (error 'num-is-odd :num x))
                                   ((evenp x) (error 'num-is-even :num x)))
                     (retry-bigger () (test (incf x)))
                     (ident () x)
                     (report-odd  () "It's odd")
                     (report-even () "It's even"))))
  (loop for i from 1 to 4
        for answer = (handler-bind ((num-is-odd #'(lambda (x) (invoke-restart 'report-odd)))
                                    (num-is-even #'(lambda (x) (invoke-restart 'report-even))))
                       (test i))
        do (format t "~a~%" answer))
  (loop for i from 1 to 4
        for answer = (handler-bind ((num-is-odd #'(lambda (x) (invoke-restart 'retry-bigger)))
                                    (num-is-even #'(lambda (x) (invoke-restart 'ident))))
                       (test i))
        do (format t "~a~%" answer)))
;> It's odd
;> It's even
;> It's odd
;> It's even        Same function called with same arguments, but different errors
;> 2                due to different surrounding handlers
;> 2
;> 4
;> 4


; The keys are:
;   * At the lowest level possible, you specify what restarts should be available
;       in the event of an error condition
;   * At a higher level, you specify what restart should be used in the event of
;       a specific error
;   * The lowest-level handler will be used, so a "catchall" can be implemented
;       high-up and a more specific one closer to the metal will over-ride it
;   * Two different functions can specify two different handlers, then call the
;       same lower-level function, and get different behaviour
