(uiop:define-package #:lw2.conditions
  (:use #:cl #:alexandria #:lw2.html-reader)
  (:export #:*debug-mode* #:condition-http-return-code
	   #:error-to-html
           #:lw2-error #:lw2-client-error #:lw2-not-found-error #:lw2-user-not-found-error #:lw2-not-allowed-error #:lw2-server-error #:lw2-connection-error #:lw2-unknown-error
	   #:log-condition #:log-conditions
	   #:log-and-ignore-errors)
  (:recycle #:lw2.backend #:lw2-viewer))

(in-package #:lw2.conditions)

(named-readtables:in-readtable html-reader)

(defvar *debug-mode* nil)

(defmethod condition-http-return-code ((c condition)) 500)

(defmethod error-to-html :around ((condition condition))
  <div class="gw-error">
    <h1>Error</h1>
    (call-next-method)
    (when *debug-mode*
      <h2>Backtrace</h2>
      <code><pre>
        (with-output-to-string (outstream)
	  (sb-debug:print-backtrace :stream outstream :from :interrupted-frame :print-frame-source t))
      </pre></code>)
  </div>)

(defmethod error-to-html ((condition condition))
  <code><pre>(princ-to-string condition)</pre></code>)

(define-condition lw2-error (error) ((http-return-code :allocation :class :reader condition-http-return-code :initform 503)))

(defmethod error-to-html ((condition lw2-error))
  <p>(princ-to-string condition)</p>)

(define-condition lw2-client-error (lw2-error) ((http-return-code :allocation :class :initform 400)))

(define-condition lw2-not-found-error (lw2-client-error) ((http-return-code :allocation :class :initform 404))
  (:report "Document not found."))

(define-condition lw2-user-not-found-error (lw2-not-found-error) ()
  (:report "User not found."))

(define-condition lw2-not-allowed-error (lw2-client-error) ((http-return-code :allocation :class :initform 403))
  (:report "LW server reports: not allowed."))

(define-condition lw2-server-error (lw2-error)
  ((message :initarg :message :reader lw2-server-error-message)
   (introduction :allocation :class :reader condition-introduction))
  (:report (lambda (c s)
	     (format s "~A:~%~A" (condition-introduction c) (lw2-server-error-message c)))))

(define-condition lw2-connection-error (lw2-server-error)
  ((introduction :allocation :class :initform "Unable to connect to LW server")))

(define-condition lw2-unknown-error (lw2-server-error)
  ((introduction :allocation :class :initform "Unrecognized LW server error")))

(defmethod error-to-html ((condition lw2-server-error))
  <p>(condition-introduction condition):</p>
  <code><pre>(lw2-server-error-message condition)</pre></code>)

(defun log-condition (condition)
  (with-open-file (outstream "./logs/error.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format outstream "~%~A: ~S ~A~%" (local-time:format-timestring nil (local-time:now)) condition condition)
    (sb-debug:print-backtrace :stream outstream :from :interrupted-frame :print-frame-source t))) 

(defmacro log-conditions (&body body)
  `(block log-conditions
     (handler-bind
	 (((or warning serious-condition) (lambda (c) (log-condition c))))
       ,@body)))

(defmacro log-and-ignore-errors (&body body)
  `(block log-and-ignore-errors
     (handler-bind
	 (((or serious-condition usocket:ns-condition usocket:socket-condition)
	   (lambda (c)
			       (log-condition c)
			       (return-from log-and-ignore-errors (values nil c)))))
       ,@body)))
