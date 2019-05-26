(uiop:define-package #:lw2.routes
  (:use #:cl #:lw2.utils)
  (:export #:route #:route-name #:standard-route #:execute-route
	   #:function-route #:no-match
	   #:regex-route))

(in-package #:lw2.routes)

(defclass route ()
  ((name :initarg :name :accessor route-name :type symbol)
   (handler :initarg :handler :type function)))

(defclass standard-route (route)
  ((uri :initarg :uri :type string)))

(defmethod execute-route ((r standard-route) request-uri)
  (with-slots (uri handler) r
    (if (string= uri request-uri)
	(progn
	  (funcall handler)
	  t)
	nil)))

(defclass function-route (route)
  ((function :initarg :function :type function)))

(define-condition no-match () ())

(defmethod execute-route ((r function-route) request-uri)
  (with-slots (function handler) r
    (handler-case
	(progn
	  (multiple-value-call handler (funcall function request-uri))
	  t)
      (no-match () nil))))

(defclass regex-route (function-route) ())

(defmethod initialize-instance :around ((r regex-route) &rest args &key regex &allow-other-keys)
  (let* ((scanner (ppcre:create-scanner regex))
	 (function
	  (lambda (request-uri)
	   (multiple-value-bind (match? strings)
	       (ppcre:scan-to-strings scanner request-uri)
	     (if match?
		 (values-list (coerce strings 'list))
		 (signal (load-time-value (make-condition 'no-match))))))))
    (apply #'call-next-method
	   r
	   :function function
	   (map-plist
	    (lambda (k v) (unless (eq k :regex) (list k v)))
	    args))))
