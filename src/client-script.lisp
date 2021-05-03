(uiop:define-package #:lw2.client-script
  (:documentation "Facilities for code that runs on both web browsers and the server.")
  (:use #:cl #:parenscript #:lw2.html-reader)
  (:import-from #:alexandria #:assoc-value)
  (:export #:client-script-function #:client-script #:client-defun
	   #:write-package-client-scripts
	   #:if-client #:when-client #:when-server
	   #:call-with-server-data
	   #:activate-client-trigger))

(in-package #:lw2.client-script)

(sb-ext:defglobal *client-script-hash* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defclass client-script-function (closer-mop:funcallable-standard-object)
  ((script :initarg :script :accessor client-script :type string))
  (:metaclass closer-mop:funcallable-standard-class))

(defmacro client-defun (name (&rest lambda-list) &body body)
  (labels ((client-test-macros (client-p body)
	     `(macrolet ((if-client (client server)
			   (declare (ignorable client server))
			   ,(if client-p 'client 'server))
			 (when-client (&body body) `(if-client (progn ,@body) nil))
			 (when-server (&body body) `(if-client nil (progn ,@body))))
		,body)))
    `(progn
       (declaim (ftype function ,name))
       (let* ((csf (make-instance 'client-script-function
				  :script (parenscript:ps ,(client-test-macros t `(defun ,name ,lambda-list ,@body))))))
	 (closer-mop:set-funcallable-instance-function csf ,(client-test-macros nil `(lambda ,lambda-list ,@body)))
	 (setf (fdefinition ',name) csf)
	 (add-client-script-to-package ',name csf *package*)))))

(defun add-client-script-to-package (name csf package)
  (setf (assoc-value (gethash package *client-script-hash*) name)
	csf))

(defun write-package-client-scripts (package stream)
  (dolist (csf-acons (gethash package *client-script-hash*))
    (write-string (client-script (cdr csf-acons)) stream)
    (terpri stream)))

(defmacro if-client (client server)
  (declare (ignore client))
  server)

(defmacro when-client (&body body)
  (declare (ignore body))
  nil)

(defmacro when-server (&body body)
  `(progn ,@body))

(defun call-with-server-data (client-function server-endpoint-uri)
  (with-html-stream-output (:stream stream)
    (format stream "<script async src=\"data:text/javascript,callWithServerData('~A','~A');\"></script>" (json:lisp-to-camel-case (string client-function)) server-endpoint-uri)))

(defun activate-client-trigger (trigger-name)
  (with-html-stream-output (:stream stream)
    (format stream "<script async src=\"data:text/javascript,activateTrigger('~A');\"></script>" trigger-name)))
