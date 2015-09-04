; Define a class with defclass
; accessor = method to read & write
; reader = read-only
; Inheritance is simple via params
(defclass animal ()
  ((speak
     :initarg :says
     :initform (error "Must supply a sound.")
     :accessor speak
     :documentation "The sound the animal makes.")
   (name
     :initarg :name
     :initform (error "Must supply a name.")
     :reader name
     :documentation "The name of the animal.")))

(defclass cat (animal)
  ((speak :initform "Meow!")))

(defclass dog (animal)
  ((speak :initform "Woof!")))

; Creation is via make-instance
(defparameter Squeak (make-instance 'cat :name "Aidy"))
(defparameter Darling (make-instance 'dog :name "Max"))

(name Squeak)
;> "Aidy"

(name Darling)
;> "Max"

(speak Squeak)
;> "Meow!"
; Can change what Squeak says, the way you'd expect
(setf (speak Squeak) "Meep!")
(speak Squeak)
;> "Meep!"
;> Alternatively, do it at creation time
(defparameter Squeak2 (make-instance 'cat :name "Aidy" :says "Meep!"))
(defparameter Darling2 (make-instance 'dog :name "Max" :says "Arf!"))
(speak Squeak2)
;> "Meep!"
(speak Darling2)
;> "Arf!"


; Classes don't have methods the way other OO langs do
; Generic functions are used instead
(defgeneric intro (a1 a2)
            (:documentation "Introduce animal a1 to animal a2"))
(defgeneric introduce (a1 a2)
            (:documentation "Animals react differently to other animals"))
; The generic function doesn't implement anything, just specifies it
; We now create methods
(defmethod introduce (a1 a2)
  ; Generic check for equality (introducing an animal to itself) that ignores types
  (if (eq a1 a2)
    (format t "Mirror?")
    (intro a1 a2)))
; If we have different animals, time to compare them in params!
; Here we define the same method repeatedly, only with a body that depends on param types
(defmethod intro ((a1 dog) (a2 dog))
  (format t "Dogpile!"))
(defmethod intro ((a1 cat) (a2 cat))
  (format t "Catfight!"))
(defmethod intro ((a1 cat) (a2 dog))
  (format t "Play?"))
(defmethod intro ((a1 dog) (a2 cat))
  (format t "onetwothreeDEATH!!!"))

; Now let's add a class of dog on vacation in France!
(defclass chien (dog)
  ((speak :initform "Woah!")))
(defmethod intro ((a1 chien) (a2 animal))
  (format t "Rabies!")
  (call-next-method))
(defmethod intro ((a1 animal) (a2 chien))
  (format t "Rabies!")
  (call-next-method))

; And test it out!
(introduce darling darling2)
;> Dogpile!
(introduce darling squeak)
;> onetwothreeDEATH!!!
(introduce squeak darling )
;> Play?
(introduce squeak squeak2)
;> Catfight!
(introduce squeak squeak)
;> Mirror?

(defparameter Harry (make-instance 'chien :name "'Arrie"))
(introduce Harry Darling)
;> Rabies!Dogpile!
(introduce Darling Harry)
;> Dogpile!
; You might expect rabies in both cases, but no! That would be the second definition
; of the intro method that handles a "chien", with it as the second parameter.
; But because the first param of that method is any animal, it is less-specific overall

; All these methods have been primaries
; There are also auxiliary methods: before; after; and around
(defgeneric bark (a1)
            (:documentation "Make a dog make some noise!"))
(defmethod bark ((a1 dog))
  (format t "~a says ~a" (name a1) (speak a1)))
(defmethod bark :around ((a1 chien))
  (format t "Le chien ")
  (call-next-method)
  (format t " And then has lunch."))
(bark darling)
;> Max says Woof!
(bark Harry)
;> Le chien 'Arrie says Woah! And then has lunch.
; Having the "rabies" be inserted by a "before" for any chiens would probably
; be the nicer way to do it, rather than munging two primaries.
