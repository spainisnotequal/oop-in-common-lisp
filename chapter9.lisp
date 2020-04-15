;;;; ----------------------------------------------
;;;; Chapter 9: Creating and initializing instances
;;;; ----------------------------------------------

;;; ------------------------------------
;;; Separation of initarg and slot names
;;; ------------------------------------

(defclass shape () ()
  (:documentation "The foundation of all shapes."))

(defclass triangle (shape)
  ((a :reader side-a)
   (b :reader side-b)
   (angle-C :reader angle-C)))

;;; Do all initialization in this method
(defmethod initialize-instance :after ((triangle triangle) &key side-a side-b side-c)
  (let* ((float-a (coerce side-a 'float))
         (float-b (coerce side-b 'float))
         (float-c (coerce side-c 'float))
         (float-angle-C (three-sides-to-angle float-c float-a float-b)))
    (with-slots (a b angle-C) triangle
      (setf a float-a)
      (setf b float-b)
      (setf angle-C float-angle-C))))

 ;;; Define the constructor
(defun make-triangle (side-a side-b side-c)
  (make-instance 'triangle :side-a side-a :side-b side-b :side-c side-c))

;;; ------------------------------
;;; Example of creating a triangle
;;; ------------------------------

(defparameter *a-triangle* (make-triangle 3 4 5))
(* 2 (angle-C *a-triangle*)) ; should be 3.1415927
