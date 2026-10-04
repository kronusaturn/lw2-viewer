(uiop:define-package #:lw2.rwlock
  (:use #:cl)
  (:import-from #:sb-thread
		#:make-mutex #:with-mutex #:grab-mutex #:release-mutex
		#:make-waitqueue #:condition-notify #:condition-broadcast #:condition-wait)
  (:import-from #:sb-ext
		#:atomic-incf #:atomic-decf)
  (:import-from #:sb-sys
		#:without-interrupts #:allow-with-interrupts #:with-interrupts)
  (:import-from #:alexandria
		#:with-gensyms
		#:once-only
		#:unwind-protect-case)
  (:export #:rwlock #:make-rwlock #:read-lock #:read-unlock #:write-lock #:write-unlock #:with-read-lock #:with-write-lock)
  (:unintern #:with-rwlock-protect))

(in-package #:lw2.rwlock)

(declaim (inline make-rwlock rwlock-readers rwlock-draining-readers))

(defstruct rwlock
  (readers 0 :type sb-ext:word)
  (draining-readers 0 :type (signed-byte 64))
  (write-mutex (make-mutex))
  (read-waitqueue-mutex (make-mutex))
  (read-waitqueue (make-waitqueue))
  (write-waitqueue-mutex (make-mutex))
  (write-waitqueue (make-waitqueue)))

(defmacro with-rwlock-accessors ((rwlock) &body body)
  `(with-accessors ,(loop for var in '(readers draining-readers write-mutex read-waitqueue-mutex read-waitqueue write-waitqueue-mutex write-waitqueue)
		       collect `(,var ,(find-symbol (format nil "~A-~A" 'rwlock var) '#:lw2.rwlock)))
       ,rwlock ,@body))

;;; States:
;;; Readers running
;;; Readers draining
;;; Writer running

(declaim (inline read-lock read-unlock))

(defun read-lock-slowpath (rwlock)
  (with-rwlock-accessors (rwlock)
    (unwind-protect-case
     ()
     (with-mutex (read-waitqueue-mutex)
       (loop until (evenp (atomic-incf readers 0))
	     do (or (condition-wait read-waitqueue read-waitqueue-mutex) (error "Waitqueue error"))))
     (:abort (read-unlock rwlock)))
    (values nil)))

(defun read-lock (rwlock)
  (with-rwlock-accessors (rwlock)
    (let ((orig-readers (atomic-incf readers 2)))
      (when (oddp orig-readers)
	(read-lock-slowpath rwlock)))
    (values nil)))

(defun read-unlock-slowpath (rwlock)
  (with-rwlock-accessors (rwlock)
    (with-mutex (write-waitqueue-mutex)
      (when (plusp draining-readers)
	(decf (the (signed-byte 61) draining-readers)))
      (when (zerop draining-readers)
	(condition-notify write-waitqueue)))
    (values nil)))

(defun read-unlock (rwlock)
  (with-rwlock-accessors (rwlock)
    (let ((orig-readers (atomic-decf readers 2)))
      (when (oddp orig-readers)
	(read-unlock-slowpath rwlock)))
    (values nil)))

(defun write-lock (rwlock &optional upgrade)
  (with-rwlock-accessors (rwlock)
    (let ((got-mutex (grab-mutex write-mutex :waitp (not upgrade))))
      (when upgrade
	(read-unlock rwlock))
      (unless got-mutex
	(return-from write-lock nil)))
    (unwind-protect-case
     ()
     (with-mutex (write-waitqueue-mutex)
       (let ((orig-readers (atomic-incf readers 1)))
	 (unless (= orig-readers 0)
	   (incf (the (signed-byte 61) draining-readers) (the (signed-byte 61) (ash orig-readers -1)))
	   (loop until (= draining-readers 0)
		 do (or (condition-wait write-waitqueue write-waitqueue-mutex) (error "Waitqueue error"))))))
     (:abort (write-unlock rwlock)))
    t))

(defun write-unlock (rwlock)
  (with-rwlock-accessors (rwlock)
    (with-mutex (read-waitqueue-mutex)
      (atomic-decf readers 1)
      (condition-broadcast read-waitqueue))
    (setf draining-readers 0)
    (release-mutex write-mutex)
    (values nil)))

(defmacro with-rwlock ((rwlock disposition) &body body)
  (multiple-value-bind (lock unlock) (ecase disposition
				       (:read (values 'read-lock 'read-unlock))
				       (:write (values 'write-lock 'write-unlock)))
    `(without-interrupts
	 (allow-with-interrupts
	  (,lock ,rwlock)
	  (unwind-protect
	       (with-interrupts ,@body)
	    (,unlock ,rwlock))))))

(defmacro with-read-lock ((rwlock &key upgrade-fn) &body body)
  (if upgrade-fn
      (with-gensyms (upgraded retry)
	`(tagbody
	  ,retry
	    (let ((,upgraded nil))
	      (flet ((,upgrade-fn ()
		       (without-interrupts
			   (allow-with-interrupts
			    (unless (write-lock ,rwlock t)
			      (go ,retry))
			    (setf ,upgraded t)))))
		(without-interrupts
		    (allow-with-interrupts
		     (read-lock ,rwlock)
		     (unwind-protect
			  (with-interrupts ,@body)
		       (if (not ,upgraded)
			   (read-unlock ,rwlock)
			   (write-unlock ,rwlock)))))))))
	`(with-rwlock (,rwlock :read) ,@body)))

(defmacro with-write-lock ((rwlock) &body body)
  `(with-rwlock (,rwlock :write) ,@body))
