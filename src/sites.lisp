(uiop:define-package #:lw2.sites
  (:use #:cl #:lw2.utils #:lw2.context #:lw2.backend-modules)
  (:import-from #:sb-ext #:defglobal)
  (:export
    #:*sites*
    #:site #:site-uri #:site-host #:site-secure #:site-backend #:site-title
    #:host-matches #:find-site
    #:call-with-site-context #:with-site-context
    #:reset-site-definitions
    #:define-site))

(in-package #:lw2.sites)

(defglobal *sites* nil)

(defclass site ()
  ((uri :accessor site-uri :initarg :uri :type simple-string)
   (host :accessor site-host :initarg :host :type simple-string)
   (secure :accessor site-secure :initarg :secure)
   (backend :accessor site-backend :initarg :backend :type backend-base)
   (title :accessor site-title :initarg :title :type simple-string)))

(defmethod host-matches ((site site) host)
  (let ((site-host (site-host site)))
    (and site-host (string-equal site-host host))))

(defun find-site (host)
  (find-if (lambda (site) (host-matches site host))
           *sites*))

(defmethod call-with-site-context ((site site) fn)
  (let ((*current-site* site)
        (*current-backend* (site-backend site)))
    (funcall fn)))

(defmacro with-site-context ((site) &body body)
  `(call-with-site-context ,site (lambda () ,@body)))

(defun reset-site-definitions ()
  (setf *sites* nil))

(defmacro define-site (&rest args)
  (let ((args2
          (map-plist (lambda (key val)
                      (cond
                        ((eq key :backend)
                         (list key `(make-backend ,@val)))
                        ((eq key :uri)
                         (let ((uri (quri:uri val)))
                           (list key val
                                 :host (quri:uri-host uri)
                                 :secure (string-equal "https" (quri:uri-scheme uri)))))
                        (t (list key val))))
                     args)))
    `(push (make-instance 'site ,.args2) *sites*)))
