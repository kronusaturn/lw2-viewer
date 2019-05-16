(uiop:define-package #:lw2.dispatchers
  (:use #:cl)
  (:export #:dispatcher #:dispatcher-name #:standard-dispatcher #:fire-dispatcher))

(in-package #:lw2.dispatchers)

(defclass dispatcher ()
  ((name :initarg :name :accessor dispatcher-name :type symbol)
   (handler :initarg :handler :type function)))

(defclass standard-dispatcher (dispatcher)
  ((uri :initarg :uri :type string)))

(defmethod fire-dispatcher ((d standard-dispatcher))
  (with-slots (uri handler) d
    (if (string= uri (hunchentoot:script-name*))
	(progn
	  (funcall handler)
	  t)
	nil)))
