(uiop:define-package #:lw2.routes
  (:use #:cl #:lw2.utils)
  (:export #:route #:route-name #:standard-route #:execute-route
	   #:function-route #:no-match
	   #:regex-route))

(in-package #:lw2.routes)

(defgeneric execute-route (route request-method request-uri))

(defclass route ()
  ((name :initarg :name :accessor route-name :type symbol)
   (method :initarg :method :accessor route-method :initform :get)
   (handler :initarg :handler :type function)))

(defmethod execute-route :around ((r route) request-method (request-uri t))
  (with-slots (method) r
    (and (or (not method) (eq method request-method))
	 (call-next-method))))

(defclass standard-route (route)
  ((uri :initarg :uri :type string)))

(defmethod execute-route ((r standard-route) (request-method t) request-uri)
  (with-slots (uri handler) r
    (if (string= uri request-uri)
	(progn
	  (funcall handler)
	  t)
	nil)))

(defclass function-route (route)
  ((function :initarg :function :type function)))

(define-condition no-match () ())

(defmethod execute-route ((r function-route) (request-method t) request-uri)
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
