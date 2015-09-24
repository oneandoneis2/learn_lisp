; Numbers (ints, floats)
; Strings
; Symbols
; Keywords
; Characters & strings
; Lists, vectors, arrays
; Hash tables

; Starting simply, integers:
1
;> 1
; Rational numbers
(/ 1 2)
;> 1/2
; floats
1.0
; doubles
1.0d0

; Strings
"foo"

; Symbols
'foo
;> FOO
'(foo |foo| \foo)
;> (FOO |foo| |fOO|)
(eq 'foo '|foo|)
;> NIL
(eq 'foo '|FOO|)
;> T

; Symbols don't have to be displayed in uppercase:
(let ((*print-case* :downcase)) (print 'foo))
;> foo
(let ((*print-case* :capitalize)) (print 'foo))
;> Foo
(let ((*print-case* :upcase)) (print 'foo))
;> FOO
; Upcase just happens to be the default



; Keywords
:foo
;> :FOO

; Characters and strings
(subseq "Hello" 0 1) ; Returns a 1-char long string
;> "H"
(char "Hello" 0) ; Returns the char itself
#\H
(let ((hello "Hello")) (setf (char hello 0) #\J) hello)
(let ((hello "Hello")) (setf (subseq hello 0 1) "J") hello)
;> "Jello" in both cases

; Vectors, like lists, are subtypes of sequences. They are integer-indexed
; Fixed-sized vectors are like C arrays
; Resizeable vectors are like Perl arrays
; Make fsv's with vector:
(vector 1 2 3)
;> #(1 2 3)
; Access elements by number:
(elt (vector 1 2 3) 0)
;> 1
(elt (vector 1 2 3) 2)
;> 3
; Other useful functions:
(count 2 (vector 1 2 3 2 1)) ; Number of occurences
;> 2
(remove 2 (vector 1 2 3 2 1)) ; Remove all occurences
;> #(1 3 1)
(remove #\l "Hello, world") ; Strings are sequences too!
;> "Heo, word"
(substitute 4 2 (vector 1 2 3 2 1)) ; Replace all occurences
;> #(1 4 3 4 1)
(find 2 (vector 1 2 3 2 1)) ; Return item, or nil if not present
;> 2
(position 2 (vector 1 2 3 2 1)) ; zero-indexed position of first instance of item
;> 1
; Modify their behaviour with keywords
(count "foo" #("foo" "bar" "baz")) ; uses eql
;> 0
(count "foo" #("foo" "bar" "baz") :test #'equal)
;> 1
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)
;> (C 30)
(remove #\l "Hello, world" :count 1)
;> "Helo, world"
(remove #\l "Hello, world" :count 1 :from-end t)
;> "Hello, word"
; Use higher-order versions that take functions
(remove-if #'evenp #(1 2 3 4))
;> #(1 3)


; Make all kinds of vector with make-array
(make-array 2)
;> #(NIL NIL)
(make-array 2 :initial-element 1)
;> #(1 1)
(make-array 2 :initial-element (vector 1 2))
;> #(#(1 2) #(1 2))
; Resizeable arrays have a max size, and contents are tracked by the fill pointer
(make-array 5 :fill-pointer 0)
;> #()
(make-array 5 :fill-pointer 5)
;> #(NIL NIL NIL NIL NIL)
; To make a truly adjustable-sized array, tell it to be adjustable:
(defparameter foo (make-array 1 :fill-pointer 0 :adjustable t))
;> #()
(vector-push "a" foo)
;> #("a")
(vector-push "b" foo)
;> #("a")
(vector-push-extend "b" foo)
;> #("a" "b")

; And then there's the hash
(defparameter *h* (make-hash-table))
(setf (gethash 'foo *h*) 'bar)
;> #S(HASH-TABLE :TEST FASTHASH-EQL (FOO . BAR))
(setf (gethash 'bar *h*) 'baz)
(maphash #'(lambda (k v) ( format t "~a: ~a~%" k v)) *h*)
;> BAR: BAZ
;> FOO: BAR
(loop for k being the hash-keys in *h* using (hash-value v)
        do ( format t "~a: ~a~%" k v))
;> BAR: BAZ
;> FOO: BAR
