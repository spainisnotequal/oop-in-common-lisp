;;;; -----------------------------------
;;;; Chapter 8: Redefining CLOS elements
;;;; -----------------------------------

;;; --------------------------
;;; The initial implementation
;;; --------------------------

(defclass shape () ()
  (:documentation "The foundation of all shapes."))

(defclass triangle (shape)
  ((a :accessor side-a :initarg :side-a)
   (b :accessor side-b :initarg :side-b)
   (c :accessor side-c :initarg :side-c)))

;;; Define the interface for making new triangles
(defun make-triangle (a b c)
  ;; All sides should be represented as floats
  (make-instance 'triangle
                 :side-a (coerce a 'float)
                 :side-b (coerce b 'float)
                 :side-c (coerce c 'float)))

;;; Return the angle A between adjacent sides b and c
;;; and opposite side a, given all sides of a triangle
;;; Law of Cosines: a^2 - b^2 + c^2 - 2bc(cos A)
(defun three-sides-to-angle (a b c)
  (acos (/ (- (+ (expt b 2)
                 (expt c 2))
              (expt a 2))
           (* 2 b c))))

;;; Return the individual angles of a triangle
(defmethod angle-A ((triangle triangle))
  (three-sides-to-angle
   (side-a triangle) (side-b triangle) (side-c triangle)))

(defmethod angle-B ((triangle triangle))
  (three-sides-to-angle
   (side-b triangle) (side-c triangle) (side-a triangle)))

(defmethod angle-C ((triangle triangle))
  (three-sides-to-angle
   (side-c triangle) (side-a triangle) (side-b triangle)))

;;; Define the generic functions that represent the interface of triangle

(defgeneric dimensions (shape)
  (:documentation "Returns list of side lengths."))

(defgeneric angles (shape)
  (:documentation "Returns list of angles."))

(defgeneric area (shape)
  (:documentation "Returns area of the shape."))

;;; Define the implementation of that interface

(defmethod dimensions ((triangle triangle))
  (list (side-a triangle) (side-b triangle) (side-c triangle)))

(defmethod angles ((triangle triangle))
  (list (angle-A triangle) (angle-B triangle) (angle-C triangle)))

;;; Return the area of a triangle ;;; Algorithm is: area = ab(sin C)/2
(defmethod area ((triangle triangle))
  (* (side-a triangle) (side-b triangle) (sin (angle-C triangle)) .5))

;;; ----------------------------------------
;;; Changing the representation of triangles
;;; ----------------------------------------

(defclass triangle (shape)
  ((a :reader side-a :initarg :side-a)
   (b :reader side-b
      :initarg :side-b)
   (angle-C :reader angle-C :initarg :angle-C)))

 ;;; Here we delete slot c and add angle-C
 ;;; We need to initialize the new slot angle-C
(defmethod update-instance-for-redefined-class :after
    ((instance triangle)
     added-slots discarded-slots
     plist &rest initargs)
  (declare (ignore initargs))
  ;; Identify this particular redefinition
  (when (and (member 'c discarded-slots)
             (member 'angle-C added-slots))
    (setf (slot-value instance 'angle-C)
          (three-sides-to-angle
           (getf plist 'c)
           (side-a instance)
           (side-b instance))))) 

;;; Write a new method to calculate the third side of a triangle.
(defmethod side-c ((triangle triangle))
  (third-side (side-a triangle) (side-b triangle) (angle-C triangle)))

;; Algorithm is: c^2 = a^2 + b^2 - 2ab(cos C)
(defun third-side (a b angle-C)
  (sqrt (- (+ (expt a 2) (expt b 2))
           (* 2 a b (cos angle-C)))))

;;; Write a new interface for making new triangles (new constructor)
(defun make-triangle (a b c)
  (let* ((float-a (coerce a 'float))
         (float-b (coerce b 'float))
         (float-c (coerce c 'float))
         (angle-C (three-sides-to-angle float-c float-a float-b)))
    (make-instance 'triangle
                   :side-a float-a
                   :side-b float-b
                   :angle-C angle-C)))
