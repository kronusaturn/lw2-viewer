(uiop:define-package #:lw2.response
    (:use #:cl #:lw2.utils #:lw2.conditions #:lw2.sites #:lw2.routes)
  (:import-from #:lw2.html-reader #:*html-output*)
  (:export #:with-response-context #:with-response-stream #:define-json-endpoint)
  (:recycle #:lw2-viewer))

(in-package #:lw2.response)

(defun call-with-response-context (fn)
  (with-site-context ((let ((host (or (hunchentoot:header-in* :x-forwarded-host) (hunchentoot:header-in* :host))))
			(or (find-site host)
			    (error "Unknown site: ~A" host))))
    (funcall fn)))

(defmacro with-response-context (() &body body)
  `(dynamic-flet ((fn () ,@body)) (call-with-response-context #'fn)))

(defun call-with-response-stream (fn)
  (unless (eq (hunchentoot:request-method*) :head)
    (let ((*html-output* (flex:make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
      (handler-case
	  (funcall fn *html-output*)
	(serious-condition () (close *html-output*))
	(:no-error (&rest x) (declare (ignore x)) (finish-output *html-output*))))))

(defmacro with-response-stream ((out-stream) &body body) `(dynamic-flet ((fn (,out-stream) ,@body)) (call-with-response-stream #'fn)))

(defun serve-json-request (fn)
  (with-response-context ()
    (let ((result
	   (handler-case (funcall fn)
	     (fatal-error (condition)
	       (setf (hunchentoot:return-code*) (condition-http-return-code condition))
	       (list-cond (t :error (princ-to-string condition))
			  #|todo (*debug-mode* :backtrace ...)|#)))))
      (setf (hunchentoot:content-type*) "application/json")
      (with-response-stream (out-stream)
	(lw2.json:encode result out-stream)))))

(defmacro define-json-endpoint ((name site-class uri) &body body)
  `(define-route ',site-class 'standard-route :name ',name :uri ,uri
		 :handler (lambda () (dynamic-flet ((fn () ,@body)) (serve-json-request #'fn)))))
