(uiop:define-package #:lw2.routes
  (:use #:cl)
  (:export #:route #:route-name #:standard-route #:execute-route))

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
