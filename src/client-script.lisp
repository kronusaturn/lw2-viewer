(uiop:define-package #:lw2.client-script
  (:documentation "Facilities for code that runs on both web browsers and the server.")
  (:use #:cl #:sb-cltl2 #:parenscript)
  (:export #:client-script-function #:client-script #:client-defun))

(in-package #:lw2.client-script)

(defclass client-script-function (closer-mop:funcallable-standard-object)
  ((script :initarg :script :accessor client-script :type string))
  (:metaclass closer-mop:funcallable-standard-class))

(defmacro client-defun (name (&rest lambda-list) &body body)
  (labels ((client-test-macros (client-p body)
	     `(macrolet ((if-client (client server)
			   (declare (ignorable client server))
			   ,(if client-p 'client 'server))
			 (when-client (&body body) `(if-client ,body (progn)))
			 (when-server (&body body) `(if-client (progn) ,body)))
		,body)))
    `(progn
       (declaim (ftype function ,name))
       (let* ((csf (make-instance 'client-script-function
				  :script (parenscript:ps ,(client-test-macros t `(defun ,name ,lambda-list ,@body))))))
	 (closer-mop:set-funcallable-instance-function csf ,(client-test-macros nil `(lambda ,lambda-list ,@body)))
	 (setf (fdefinition ',name) csf)))))
