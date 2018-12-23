(uiop:define-package #:lw2.backend-modules
  (:use #:cl)
  (:import-from #:alexandria #:symbolicate)
  (:export
    #:backend-base
    #:backend-lmdb-cache #:backend-lmdb-environment #:backend-cache-db-path
    #:backend-graphql
    #:backend-websocket-login
    #:graphql-uri #:websocket-uri #:algolia-search-uri
    #:backend-q-and-a
    #:backend-lw2-legacy #:backend-lw2-modernized #:backend-lw2 #:backend-ea-forum #:backend-accordius
    #:make-backend #:define-backend-function #:define-backend-operation #:backend)
  (:unintern #:declare-backend-function)
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

(defclass backend-algolia-search (backend-base)
  ((algolia-search-uri :accessor algolia-search-uri :initarg :algolia-search-uri :type simple-string)))

(defclass backend-q-and-a (backend-graphql) ())

(defclass backend-lw2-legacy (backend-graphql) ())

(defclass backend-lw2-modernized (backend-graphql) ())

(defclass backend-lw2 (backend-websocket-login backend-lw2-modernized backend-lw2-legacy backend-algolia-search backend-q-and-a) ())

(defclass backend-ea-forum (backend-websocket-login backend-lw2-modernized backend-lw2-legacy backend-algolia-search) ())

(defclass backend-accordius (backend-lw2-modernized backend-lw2-legacy) ())

(defun make-backend (type-string &rest args)
  (apply #'make-instance (symbolicate "BACKEND-" (string-upcase type-string)) args))

(defmacro define-backend-function (name lambda-list)
  (let ((inner-name (symbolicate "%" name))
        (lambda-list (map 'list (lambda (x) (if (atom x) x (first x))) lambda-list)))
   `(progn
      (export '(,name ,inner-name))
      (defgeneric ,inner-name (backend ,@lambda-list))
      (defmacro ,name (&rest args) (list* ',inner-name 'lw2.context:*current-backend*  args)))))

(defmacro define-backend-operation (name backend &rest args)
  (let* ((inner-name (symbolicate "%" name))
         (latter-args (member-if #'listp args))
         (method-qualifiers (ldiff args latter-args))
         (method-args (first latter-args))
         (body (rest latter-args)))
    `(defmethod ,inner-name ,.method-qualifiers ((backend ,backend) ,@method-args) ,@body)))
