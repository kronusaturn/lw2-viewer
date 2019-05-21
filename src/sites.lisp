(uiop:define-package #:lw2.sites
  (:use #:cl #:lw2.utils #:lw2.context #:lw2.routes #:lw2.backend-modules #:lw2.fonts-modules)
  (:import-from #:sb-ext #:defglobal)
  (:export
    #:*sites*
    #:site #:forum-site #:wiki-site
    #:alternate-frontend-site #:lesswrong-viewer-site #:ea-forum-viewer-site
    #:arbital-site
    #:site-class #:call-route-handler #:site-class-routes
    #:site-uri #:site-host #:site-secure #:site-backend #:site-title #:site-description #:background-loader-enabled #:site-fonts-source
    #:main-site-title #:main-site-abbreviation #:main-site-uri
    #:host-matches #:find-site
    #:call-with-site-context #:with-site-context
    #:reset-site-definitions
    #:define-site))

(in-package #:lw2.sites)

(defglobal *sites* nil)

(defclass site-class (standard-class)
  ((routes :accessor site-class-routes :initform nil)))

(defmethod closer-mop:validate-superclass ((c site-class) (sc standard-class))
  t)

(defmethod site-class-routes ((c t))
  nil)

(defmethod call-route-handler ((original-class site-class) request-uri)
  (dolist (class (closer-mop:class-precedence-list original-class))
    (dolist (route (site-class-routes class))
      (when (execute-route route request-uri)
	(return-from call-route-handler t)))))

(defclass site ()
  ((uri :accessor site-uri :initarg :uri :type simple-string)
   (host :accessor site-host :initarg :host :type simple-string)
   (secure :accessor site-secure :initarg :secure)
   (backend :accessor site-backend :initarg :backend :type backend-base)
   (title :accessor site-title :initarg :title :type simple-string)
   (description :accessor site-description :initarg :description :type simple-string)
   (background-loader-enabled :accessor background-loader-enabled :initarg :use-background-loader :initform nil :type boolean)
   (fonts-source :accessor site-fonts-source :initarg :fonts-source :initform (make-instance 'google-fonts-source) :type fonts-source))
  (:metaclass site-class))

(defmethod main-site-title ((s site)) nil)

(defmethod main-site-abbreviation ((s site)) nil)

(defmethod call-route-handler ((s site) request-uri)
  (call-route-handler (class-of s) request-uri))

(defclass forum-site (site) ()
  (:metaclass site-class))

(defclass wiki-site (site) ()
  (:metaclass site-class))

(defclass alternate-frontend-site (site)
  ((main-site-title :accessor main-site-title :initarg :main-site-title :type simple-string)
   (main-site-abbreviation :accessor main-site-abbreviation :initarg :main-site-abbreviation :type simple-string)
   (main-site-uri :accessor main-site-uri :initarg :main-site-uri :type simple-string))
  (:metaclass site-class))

(defclass lesswrong-viewer-site (forum-site alternate-frontend-site) ()
  (:metaclass site-class))

(defclass ea-forum-viewer-site (forum-site alternate-frontend-site) ()
  (:metaclass site-class))

(defclass arbital-site (wiki-site alternate-frontend-site) ()
  (:metaclass site-class))

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
                                  (port (quri:uri-port uri))
                                  (default-port (quri.port:scheme-default-port scheme)))
                             (list key val
                                   :host (format nil "~A~@[:~A~]"
                                                 host
                                                 (if (/= default-port port) port))
                                   :secure (string-equal "https" scheme))))
                          (t (list key val))))
                      args)))
    `(push (make-instance ',class ,.args2) *sites*)))
