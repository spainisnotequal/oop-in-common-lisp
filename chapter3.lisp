;;;; --------------------------------------------------
;;;; Chapter 3: Developing a Simple CLOS Program: Locks
;;;; --------------------------------------------------


;;; Define classes

(defclass lock ()
  ((name :initarg :name
         :reader lock-name))
  (:documentation "The foundation of all locks."))

(defclass null-lock (lock)
  ()
  (:documentation "A lock that is always free."))

(defclass simple-lock (lock)
  ((owner :initform nil
          :accessor lock-owner))
  (:documentation "A lock that is either free or busy."))

;;; Define constructors in order to avoid using "make-instance"

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

;;; Create new objects - instances

(defparameter *null-lock*
  (make-null-lock "Null lock"))

(defparameter *simple-lock*
  (make-simple-lock "Simple lock"))

;;; Access slots

(lock-name *null-lock*) ; => "Null lock"
(lock-name *simple-lock*) ; => "Simple lock"

(lock-owner *null-lock*) ; => There is no applicable method for the generic function
(lock-owner *simple-lock*) ; => NIL

;;; Set up slots

(setf (lock-owner *simple-lock*) 3401) ; => 3401
(lock-owner *simple-lock*) ; => 3401

;;; Query a lock for its type

(type-of *null-lock*) ; => null-lock
(type-of *simple-lock*) ; => simple-lock

(typep *simple-lock* 'simple-lock) ; => T
(typep *simple-lock* 'lock) ; => T
(typep *simple-lock* 't) ; => T

;;; Check classes relationships

(subtypep 'null-lock 'lock) ; => T, T
(subtypep 'simple-lock 'lock) ; => T, T
(subtypep 'null-lock 'simple-lock) ; => NIL, T
