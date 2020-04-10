;;;; --------------------------------------------------
;;;; Chapter 3: Developing a Simple CLOS Program: Locks
;;;; --------------------------------------------------

;;; --------------
;;; Define classes
;;; --------------

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

;;; -----------------------------------------------------------
;;; Define constructors in order to avoid using "make-instance"
;;; (constructors are part of the interface)
;;; -----------------------------------------------------------

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

;;; ------------------------------
;;; Create new objects - instances
;;; ------------------------------

(defparameter *null-lock*
  (make-null-lock "Null lock"))

(defparameter *simple-lock*
  (make-simple-lock "Simple lock"))
;;; ------------
;;; Access slots
;;; ------------

(lock-name *null-lock*) ; => "Null lock"
(lock-name *simple-lock*) ; => "Simple lock"

(lock-owner *null-lock*) ; => There is no applicable method for the generic function
(lock-owner *simple-lock*) ; => NIL

;;; ------------
;;; Set up slots
;;; ------------

(setf (lock-owner *simple-lock*) 3401) ; => 3401
(lock-owner *simple-lock*) ; => 3401

;;; -------------------------
;;; Query a lock for its type
;;; -------------------------

(type-of *null-lock*) ; => null-lock
(type-of *simple-lock*) ; => simple-lock

(typep *simple-lock* 'simple-lock) ; => T
(typep *simple-lock* 'lock) ; => T
(typep *simple-lock* 't) ; => T

;;; ---------------------------
;;; Check classes relationships
;;; ---------------------------

(subtypep 'null-lock 'lock) ; => T, T
(subtypep 'simple-lock 'lock) ; => T, T
(subtypep 'null-lock 'simple-lock) ; => NIL, T

;;; ------------------------------------------------------------
;;; Define the (rest of the) interface through generic functions
;;; ------------------------------------------------------------

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

;;; -----------------------
;;; Methods for null locks
;;; -----------------------
 
(defmethod seize ((lock null-lock))
  lock) ; return lock, no waiting

(defmethod release ((lock null-lock) &optional failure-mode)
  (declare (ignore failure-mode)) ; never fails for null locks
  lock)

;;; -------------------
;;; Locks and processes
;;; -------------------

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


;;; ------------------------
;;; Methods for simple locks
;;; ------------------------

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

;;; ------------------------------------
;;; Specializing print-objects for Locks
;;; ------------------------------------

(defmethod print-object ((lock lock) stream)
  (format stream "#<~S ~A ~D>"
          (type-of lock)
          (if (slot-boundp lock 'name)
              (lock-name lock)
              "(no name)")
          (sb-kernel:get-lisp-obj-address lock))) ; get the memory address of lock (SBCL function, not standard)

;;; -------------------------------
;;; Specializing describe for Locks
;;; -------------------------------

(defmethod describe ((lock lock))
  (format t "~&~S is a lock of type ~S named '~A'."
          lock
          (type-of lock)
          (if (slot-boundp lock 'name)
              (lock-name lock)
              "(no name)"))
  (values))

;;; -----------------------------------------------------------
;;; Specializing describe for Simple Locks with an after method
;;; -----------------------------------------------------------

(defmethod describe :after ((lock simple-lock))
  (let ((owner (lock-owner lock)))
    (format t (if owner
                  "~&It is now owned by process ~A.~%"
                  "~&It is now free.~%")
            owner)))

;;; ----------------------
;;; Defining a mixin class
;;; ----------------------

(defclass ordered-lock-mixin ()
  ((level :initarg :level
          :reader lock-level
          :type integer))
  (:documentation "Avoids deadlock by checking lock order."))

;;; ---------------------------
;;; Defining aggreagate classes
;;; ---------------------------

(defclass ordered-lock (ordered-lock-mixin simple-lock)
  ()
  (:documentation
"Avoids deadlock by ensuring that a process seizes locks in a specific order.
When seizing, waits if the lock is busy."))

(defclass ordered-null-lock (ordered-lock-mixin null-lock)
  ()
  (:documentation
"Avoids deadlock by ensuring that a process seizes locks in a specific order.
Does not actually seize anything, but does check that the lock ordering is obeyed."))

;;; -------------------
;;; Define constructors
;;; -------------------

(defun make-ordered-null-lock (name level)
  (make-instance 'ordered-null-lock
                 :name name
                 :level level))

(defun make-ordered-lock (name level)
  (make-instance 'ordered-lock
                 :name name
                 :level level))

;;; ---------------------------------------
;;; Specialize "describe" for ordered locks
;;; ---------------------------------------

(defmethod describe :after ((lock ordered-lock-mixin))
  (format t "~&Its lock level is ~D." (lock-level lock)))

(defparameter *lock-C* (make-ordered-lock "C" 3))

(describe *lock-C*)

;;; -----------------------------------
;;; Implement ordered locking behaviour
;;; -----------------------------------

(defparameter *process-lock-table* (make-hash-table)
  "Each key is a process identifier;
   value is a list of ordered locks it owns.")

(defun add-process-lock (process lock)
  (without-process-preemtion
      (push lock
            (gethash process *process-lock-table*))))

(defun delete-process-lock (process lock)
  (without-process-preemtion
      (let ((hash-entry (gethash process *process-lock-table*)))
        (setf (gethash process *process-lock-table*) (delete lock hash-entry)))))

(defun get-process-locks (process)
  (without-process-preemtion
      (gethash process *process-lock-table*)))

(defun get-highest-lock (process)
  (first (get-process-locks process)))

;;; -------------------------
;;; Methods for ordered locks
;;; -------------------------

(defmethod seize :before ((lock ordered-lock-mixin))
  "Checks validity of this process seizing this ordered lock.
  If invalid, signals an error.
  If valid, does nothing and allows primary method to run."
  ;; First check for the mylock mistake to give the specific
  ;; error for that case, instead of the "Out of order" error.
  (check-for-mylock lock *current-process*)
  ;; Now check for a possible infraction of ordered locking.
  (let ((highest-lock (get-highest-lock *current-process*)))
    (when (and highest-lock
               (<= (lock-level lock) (lock-level highest-lock)))
      (error "Out of order: Can't seize ~A while owning ~A" lock highest-lock))))

(defmethod seize :after ((lock ordered-lock-mixin))
  "Adds the lock to the *process-lock-table*."
  (add-process-lock *current-process* lock))

;;; ----------------------
;;; Locking a shared queue
;;; ----------------------

(defclass print-request-queue ()
  ((lock :accessor print-queue-lock :initform (make-simple-lock "Print Queue"))
   (requests :accessor print-requests :initform nil))
  (:documentation "Queue of pending print requests."))

(defun make-print-queue ()
  (make-instance 'print-request-queue))

(defparameter *print-queue* (make-print-queue))

(defun enqueue-print-request (request)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
         (progn (seize lock)
                (push request (print-requests *print-queue*)))
      (release lock :no-error))))

(defun dequeue-print-request (request)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
         (progn
           (seize lock)
           (setf (print-requests *print-queue*)
                 (delete request (print-requests *print-queue*))))
      (release lock :no-error))))
