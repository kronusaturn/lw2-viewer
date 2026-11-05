(uiop:define-package #:lw2.async-buffer
    (:use #:cl)
  (:export #:make-async-buffer #:read-async-buffer #:write-async-buffer))

(cl:in-package #:lw2.async-buffer)

(defstruct async-buffer
  (data nil :type t)
  (ready nil :type boolean)
  (mutex (sb-thread:make-mutex))
  (waitqueue (sb-thread:make-waitqueue)))

(defmacro with-maybe-deadline ((&key seconds) &body body)
  `(flet ((fn () ,@body))
     (let ((seconds ,seconds))
       (if seconds
	   (sb-sys:with-deadline (:seconds seconds) (fn))
	   (fn)))))

(defun read-async-buffer (b &key timeout)
  (unless (async-buffer-ready b)
    (with-maybe-deadline (:seconds timeout)
      (sb-thread:with-mutex ((async-buffer-mutex b))
	(loop until (async-buffer-ready b)
	      do (or (sb-thread:condition-wait (async-buffer-waitqueue b) (async-buffer-mutex b))
		     (error "Waitqueue error."))))))
  (sb-thread:barrier (:read))
  (async-buffer-data b))

(defun write-async-buffer (b data)
  (setf (async-buffer-data b) data)
  (sb-thread:barrier (:write))
  (setf (async-buffer-ready b) t)
  (sb-thread:with-mutex ((async-buffer-mutex b))
    (sb-thread:condition-broadcast (async-buffer-waitqueue b))))
