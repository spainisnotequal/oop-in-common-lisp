;;;; -------------------------------------------
;;;; Chapter 4: Controlling the generic dispacth
;;;; -------------------------------------------

;;; --------------------------------------------
;;; Implementation choices: methods versus slots
;;; --------------------------------------------

;;; Define the class triangle
(defclass triangle (shape)
  ((side-a :accessor side-a :initarg :side-a)
   (side-b :accessor side-b :initarg :side-b)
   (side-c :accessor side-c :initarg :side-c)
   (number-of-sides :reader number-of-sides
                    :initform 3
                    :allocation :class)))

;;; Return the area of a triangle, given three sides.
;;; Algorithm is: area - ab(sin C)/2
(defun area-of-triangle (a b c)
  (let ((angle-C (three-sides-to-angle c a b)))
    (* a b (sin angle-C) .5)))

;;; Return the angle A between adjacent sides b and c and opposite side a,
;;; given all sides of a triangle.
;;; Law of Cosines: aA2 - bA2 + cA2 - 2bc(cos A)
(defun three-sides-to-angle (a b c)
  (acos (/ (- (+ (expt b 2) (expt c 2))
              (expt a 2))
           (* 2 b c))))

;;; Return the area of a triangle.
(defmethod area ((tri triangle))
  (area-of-triangle (side-a tri)
                    (side-b tri)
                    (side-c tri)))

;;; Redefine the class triangle
(defclass triangle (shape)
  ((side-a :accessor side-a :initarg :side-a)
   (side-b :accessor side-b :initarg :side-b)
   (side-c :accessor side-c :initarg :side-c)
   (number-of-sides :reader number-of-sides
                    :initform 3
                    :allocation :class)
   (area :reader area :initarg :area)))

;;; Define a constructor for the class triangle
(defun make-triangle (side-a side-b side-c)
  (make-instance 'triangle
                 :side-a side-a
                 :side-b side-b
                 :side-c side-c
                 :area (area-of-triangle side-a side-b side-c)))

;;; Represent information about a class with a method instead that with a slot
(defmethod number-of-sides ((tri triangle))
  3)
