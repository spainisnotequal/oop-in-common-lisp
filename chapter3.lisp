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
;;; (constructors are part of the interface)

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

;;; Define the (rest of the) interface through generic functions
(defgeneric seize (lock)
  (:documentation
"Seizes the lock.
Returns the lock when operation succeeds.
Some locks simply wait until they can succeed, while other locks return NIL if they fail."))

(defgeneric release (lock &optional failure-mode)
  (:documentation
"Releases the lock if it is currently owned by this process.
Returns T if the operation succedds.
If unsuccessful and failure-mode is :no-error, returns NIL.
If unsuccessful and failure-mode is :error, signals an error.
The default for failure-mode is :no-error."))

;;; Methods for null locks
(defmethod seize (lock null-lock)
  lock) ; return lock, no waiting

(defmethod release ((lock null-lock) &optional failure-mode)
  (declare (ignore failure-mode)) ; never fails for null locks
  lock)

;;; Locks anbd processes

;;  (we assume that the three following primitives exist:
;;    without-process-preemtion &body body
;;    process-wait reason function &rest arguments
;;    *current-process*

;; If value of place is old-value, set it to new-value
;; Return T if the setf worked, NIL otherwise
(defmacro setf-if (place old-value new-value)
  `(without-process-preemtion ; do automatically
       (cond ((eql ,place ,old-value)
              (setf ,place ,new-value)
              t)
             (t nil))))


;;; Methods for simple locks
(defmethod check-for-mylock ((lock simple-lock) process)
  (when (eql (lock-owner lock) process)
    (error "Can't seize ~A because you already own it." lock)))

(defmethod seize ((lock simple-lock))
  (check-for-mylock lock *current-process*)
  (do ()
      ((setf-if (lock-owner lock) nil *current-process*))
    (process-wait "Seizing lock"
                  #'(lambda () (null (lock-owner lock))))))

(defmethod release ((lock simple-lock)
                    &optional (failure-mode :no-error))
  (or (setf-if (lock-owner lock) *current-process* nil)
      (ecase failure-mode
        (:no-error nil)
        (:error (error "~A is not owned by this process." lock)))))
