(uiop:define-package #:lw2.sites
  (:use #:cl #:lw2.utils #:lw2.context #:lw2.backend-modules)
  (:import-from #:sb-ext #:defglobal)
  (:export
    #:*sites*
    #:site #:lesswrong-viewer-site #:ea-forum-viewer-site
    #:site-uri #:site-host #:site-secure #:site-backend #:site-title
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

(defclass lesswrong-viewer-site (site) ())

(defclass ea-forum-viewer-site (site) ())

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
  (let* ((class 'site)
         (args2
           (map-plist (lambda (key val)
                        (cond
                          ((eq key :class)
                           (setf class val)
                           nil)
                          ((eq key :backend)
                           (list key `(make-backend ,@val)))
                          ((eq key :uri)
                           (let* ((uri (quri:uri val))
                                  (scheme (quri:uri-scheme uri))
                                  (host (quri:uri-host uri))
                                  (port (quri:uri-port uri)))
                             (list key val
                                   :host (format nil "~A~@[:~A~]"
                                                 (quri:uri-host uri)
                                                 (if (/= (quri.port:scheme-default-port scheme) port) port))
                                   :secure (string-equal "https" (quri:uri-scheme uri)))))
                          (t (list key val))))
                      args)))
    `(push (make-instance ',class ,.args2) *sites*)))
