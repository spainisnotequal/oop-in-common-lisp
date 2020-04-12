;;;; -------------------------------------------
;;;; Chapter 4: Controlling the generic dispacth
;;;; -------------------------------------------

;;; --------------------------------------------
;;; Implementation choices: methods versus slots
;;; --------------------------------------------

;; Define the class triangle
(defclass triangle (shape)
  ((side-a :accessor side-a :initarg :side-a)
   (side-b :accessor side-b :initarg :side-b)
   (side-c :accessor side-c :initarg :side-c)
   (number-of-sides :reader number-of-sides
                    :initform 3
                    :allocation :class)))

;; Return the area of a triangle, given three sides.
;; Algorithm is: area - ab(sin C)/2
(defun area-of-triangle (a b c)
  (let ((angle-C (three-sides-to-angle c a b)))
    (* a b (sin angle-C) .5)))

;; Return the angle A between adjacent sides b and c and opposite side a,
;; given all sides of a triangle.
;; Law of Cosines: aA2 - bA2 + cA2 - 2bc(cos A)
(defun three-sides-to-angle (a b c)
  (acos (/ (- (+ (expt b 2) (expt c 2))
              (expt a 2))
           (* 2 b c))))

;; Return the area of a triangle.
(defmethod area ((tri triangle))
  (area-of-triangle (side-a tri)
                    (side-b tri)
                    (side-c tri)))

;; Redefine the class triangle
(defclass triangle (shape)
  ((side-a :accessor side-a :initarg :side-a)
   (side-b :accessor side-b :initarg :side-b)
   (side-c :accessor side-c :initarg :side-c)
   (number-of-sides :reader number-of-sides
                    :initform 3
                    :allocation :class)
   (area :reader area :initarg :area)))

;; Define a constructor for the class triangle
(defun make-triangle (side-a side-b side-c)
  (make-instance 'triangle
                 :side-a side-a
                 :side-b side-b
                 :side-c side-c
                 :area (area-of-triangle side-a side-b side-c)))

;; Represent information about a class with a method instead that with a slot
(defmethod number-of-sides ((tri triangle))
  3)

;;; --------------------------
;;; Programming with accessors
;;; --------------------------

;; Define an auxiliary (after) method for the writer accessor
(defmethod (setf side-a) :after (new-length (tri triangle))
  (setf (area tri)(area-of-triangle new-length
                                    (side-b tri)
                                    (side-c tri))))

;; Defining the reader side-a
(defmethod side-a ((tri triangle))
  (slot-value tri 'side-a))

;; Defining the writer (setf side-a)
(defmethod (setf side-a) (new-side-a (tri triangle))
  (setf (slot-value tri 'side-a) new-side-a))

;;; with-accessor versus readers

;; using the readers:
(defmethod angle-A ((triangle triangle))
  (three-sides-to-angle (side-a triangle)
                        (side-b triangle)
                        (side-c triangle)))

;; using with-accessors:
(defmethod angle-A ((triangle triangle))
  (with-accessors ((a side-a)
                   (b side-b)
                   (c side-c))
      triangle
    (three-sides-to-angle a b c)))

;;; with-slots versus access the slots directly (through slot-value)

;; using slot-value
(defmethod angle-A ((triangle triangle))
  (three-sides-to-angle (slot-value triangle 'side-a)
                        (slot-value triangle 'side-b)
                        (slot-value triangle 'side-c)))

;; using with-slots
(defmethod angle-A ((triangle triangle))
  (with-slots (side-a side-b side-c)
      triangle
    (three-sides-to-angle side-a side-b side-c)))

