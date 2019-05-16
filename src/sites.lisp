(uiop:define-package #:lw2.sites
  (:use #:cl #:lw2.utils #:lw2.context #:lw2.dispatchers #:lw2.backend-modules #:lw2.fonts-modules)
  (:import-from #:sb-ext #:defglobal)
  (:export
    #:*sites*
    #:site #:alternate-frontend-site #:lesswrong-viewer-site #:ea-forum-viewer-site
    #:dispatcher-class #:iterate-dispatchers #:class-dispatchers
    #:site-uri #:site-host #:site-secure #:site-backend #:site-title #:site-description #:background-loader-enabled #:site-fonts-source
    #:main-site-title #:main-site-abbreviation #:main-site-uri
    #:host-matches #:find-site
    #:call-with-site-context #:with-site-context
    #:reset-site-definitions
    #:define-site))

(in-package #:lw2.sites)

(defglobal *sites* nil)

(defclass dispatcher-class (standard-class)
  ((dispatchers :accessor class-dispatchers :initform nil)))

(defmethod closer-mop:validate-superclass ((c dispatcher-class) (sc standard-class))
  t)

(defmethod class-dispatchers ((c t))
  nil)

(defmethod iterate-dispatchers ((original-class dispatcher-class))
  (dolist (class (closer-mop:class-precedence-list original-class))
    (dolist (dispatcher (class-dispatchers class))
      (when (fire-dispatcher dispatcher)
	(return-from iterate-dispatchers t)))))

(defclass site ()
  ((uri :accessor site-uri :initarg :uri :type simple-string)
   (host :accessor site-host :initarg :host :type simple-string)
   (secure :accessor site-secure :initarg :secure)
   (backend :accessor site-backend :initarg :backend :type backend-base)
   (title :accessor site-title :initarg :title :type simple-string)
   (description :accessor site-description :initarg :description :type simple-string)
   (background-loader-enabled :accessor background-loader-enabled :initarg :use-background-loader :initform nil :type boolean)
   (fonts-source :accessor site-fonts-source :initarg :fonts-source :initform (make-instance 'google-fonts-source) :type fonts-source))
  (:metaclass dispatcher-class))

(defmethod main-site-title ((s site)) nil)

(defmethod main-site-abbreviation ((s site)) nil)

(defmethod iterate-dispatchers ((s site))
  (iterate-dispatchers (class-of s)))

(defclass forum-site (site) ()
  (:metaclass dispatcher-class))

(defclass wiki-site (site) ()
  (:metaclass dispatcher-class))

(defclass alternate-frontend-site (site)
  ((main-site-title :accessor main-site-title :initarg :main-site-title :type simple-string)
   (main-site-abbreviation :accessor main-site-abbreviation :initarg :main-site-abbreviation :type simple-string)
   (main-site-uri :accessor main-site-uri :initarg :main-site-uri :type simple-string))
  (:metaclass dispatcher-class))

(defclass lesswrong-viewer-site (forum-site alternate-frontend-site) ()
  (:metaclass dispatcher-class))

(defclass ea-forum-viewer-site (forum-site alternate-frontend-site) ()
  (:metaclass dispatcher-class))

(defclass arbital-site (forum-site alternate-frontend-site) ()
  (:metaclass dispatcher-class))

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
