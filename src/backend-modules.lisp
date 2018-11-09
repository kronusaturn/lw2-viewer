(uiop:define-package #:lw2.backend-modules
  (:use #:cl)
  (:import-from #:alexandria #:symbolicate)
  (:export
    #:backend-base
    #:backend-lmdb-cache #:backend-lmdb-environment #:backend-cache-db-path
    #:backend-graphql
    #:graphql-uri #:websocket-uri
    #:backend-lw2-legacy #:backend-lw2-modernized #:backend-lw2 #:backend-accordius
    #:make-backend #:declare-backend-function #:define-backend-operation #:backend)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.backend-modules)

(defclass backend-base () ())

(defclass backend-lmdb-cache (backend-base)
  ((lmdb-environment :accessor backend-lmdb-environment :initform nil)
   (cache-db-path :accessor backend-cache-db-path :initarg :cache-db-path :type simple-string)))

(defclass backend-graphql (backend-lmdb-cache)
  ((graphql-uri :accessor graphql-uri :initarg :graphql-uri :type simple-string)))

(defclass backend-websocket-login (backend-base)
  ((websocket-uri :accessor websocket-uri :initarg :websocket-uri :type simple-string)))

(defclass backend-lw2-legacy (backend-graphql) ())

(defclass backend-lw2-modernized (backend-graphql) ())

(defclass backend-lw2 (backend-websocket-login backend-lw2-modernized backend-lw2-legacy) ())

(defclass backend-accordius (backend-lw2-modernized backend-lw2-legacy) ())

(defun make-backend (type-string &rest args)
  (apply #'make-instance (symbolicate "BACKEND-" (string-upcase type-string)) args))

(defmacro declare-backend-function (name)
  (let ((inner-name (symbolicate "%" name)))
   `(progn
      (export '(,name ,inner-name))
      (defmacro ,name (&rest args) (list* ',inner-name 'lw2.context:*current-backend*  args)))))

(defmacro define-backend-operation (name backend &rest args)
  (let* ((inner-name (symbolicate "%" name))
         (latter-args (member-if #'listp args))
         (method-qualifiers (ldiff args latter-args))
         (method-args (first latter-args))
         (body (rest latter-args)))
    `(defmethod ,inner-name ,.method-qualifiers ((backend ,backend) ,@method-args) ,@body)))
