(uiop:define-package #:lw2.backend-modules
  (:use #:cl)
  (:import-from #:alexandria #:symbolicate)
  (:export
    #:backend-class #:class-cached-databases #:class-own-databases #:class-databases-epoch
    #:backend-base
    #:backend-dexador-connection-pool #:backend-dexador-connection-pools #:backend-dexador-connection-pools-lock
    #:backend-lmdb-cache #:backend-lmdb-environment #:backend-cache-db-path
    #:backend-graphql
    #:backend-websocket-login
    #:graphql-uri #:websocket-uri #:algolia-search-uri #:rest-api-uri
    #:backend-feed-crossposts
    #:backend-q-and-a #:backend-related-questions
    #:backend-alignment-forum
    #:backend-events
    #:backend-shortform
    #:backend-backlinks
    #:backend-push-notifications
    #:backend-lw2-tags
    #:backend-lw2-misc-features
    #:backend-lw2-legacy #:backend-lw2-modernized #:backend-lw2 #:backend-algolia-search #:backend-ea-forum #:backend-accordius
    #:backend-arbital
    #:make-backend #:define-backend-function #:define-backend-operation #:backend)
  (:unintern #:declare-backend-function)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.backend-modules)

(defclass backend-class (standard-class)
  ((cached-databases :accessor class-cached-databases :initform nil)
   (own-databases :accessor class-own-databases :initform nil)
   (databases-epoch :accessor class-databases-epoch :initform 0)))

(defmethod closer-mop:validate-superclass ((c backend-class) (sc standard-class))
  t)

(defclass backend-base () () (:metaclass backend-class))

(defclass backend-dexador-connection-pool (backend-base)
  ((dexador-connection-pools :accessor backend-dexador-connection-pools :initform nil)
   (dexador-connection-pools-lock :accessor backend-dexador-connection-pools-lock :initform (sb-thread:make-mutex :name "Dexador connection pools mutex")))
  (:metaclass backend-class))

(defclass backend-lmdb-cache (backend-base)
  ((lmdb-environment :accessor backend-lmdb-environment :initform nil)
   (cache-db-path :accessor backend-cache-db-path :initarg :cache-db-path :type simple-string))
  (:metaclass backend-class))

(defclass backend-graphql (backend-lmdb-cache backend-dexador-connection-pool)
  ((graphql-uri :accessor graphql-uri :initarg :graphql-uri :type simple-string))
  (:metaclass backend-class))

(defclass backend-websocket-login (backend-base)
  ((websocket-uri :accessor websocket-uri :initarg :websocket-uri :type simple-string))
  (:metaclass backend-class))

(defclass backend-algolia-search (backend-base)
  ((algolia-search-uri :accessor algolia-search-uri :initarg :algolia-search-uri :type simple-string))
  (:metaclass backend-class))

(defclass backend-feed-crossposts (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-q-and-a (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-related-questions (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-backlinks (backend-lmdb-cache) ()
  (:metaclass backend-class))

(defclass backend-push-notifications (backend-lmdb-cache) ()
  (:metaclass backend-class))

(defclass backend-alignment-forum (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-events (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-shortform (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-lw2-tags (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-lw2-misc-features (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-lw2-legacy (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-lw2-modernized (backend-graphql) ()
  (:metaclass backend-class))

(defclass backend-lw2 (backend-websocket-login
		       backend-lw2-modernized
		       backend-lw2-legacy
		       backend-lw2-misc-features
		       backend-algolia-search
		       backend-q-and-a
		       backend-related-questions
		       backend-alignment-forum
		       backend-events
		       backend-feed-crossposts
		       backend-backlinks
		       backend-push-notifications
		       backend-shortform
		       backend-lw2-tags) ()
  (:metaclass backend-class))

(defclass backend-ea-forum (backend-websocket-login
			    backend-lw2-modernized
			    backend-lw2-legacy
			    backend-lw2-misc-features
			    backend-algolia-search
			    backend-q-and-a
			    backend-related-questions
			    backend-feed-crossposts
			    backend-backlinks
			    backend-push-notifications
			    backend-shortform) ()
  (:metaclass backend-class))

(defclass backend-accordius (backend-lw2-legacy backend-lw2-modernized)
  ((rest-api-uri :accessor rest-api-uri :initarg :rest-api-uri :type simple-string))
  (:metaclass backend-class))

(defclass backend-arbital (backend-lmdb-cache backend-dexador-connection-pool) ()
  (:metaclass backend-class))

(defun make-backend (type-string &rest args)
  (apply #'make-instance (symbolicate "BACKEND-" (string-upcase type-string)) args))

(defun process-operation-definition (args)
  (let* ((latter-args (member-if #'listp args))
	 (method-qualifiers (ldiff args latter-args))
	 (method-args (first latter-args))
	 (body (rest latter-args)))
    (values method-qualifiers method-args body)))

(defmacro define-backend-function (name lambda-list &rest operations)
  (let ((inner-name (symbolicate "%" name))
        (lambda-list (map 'list (lambda (x) (if (atom x) x (first x))) lambda-list))
	(method-definitions
	 (mapcar (lambda (op)
		   (destructuring-bind (backend &rest body) op
		     `(:method ((backend ,backend) ,@lambda-list) ,@body)))
		 operations)))
   `(progn
      (export '(,name ,inner-name))
      (defgeneric ,inner-name (backend ,@lambda-list) ,.method-definitions)
      (declaim (inline ,name))
      (defun ,name (&rest args) (apply ',inner-name lw2.context:*current-backend* args)))))

(defmacro define-backend-operation (name backend &rest args)
  (let* ((inner-name (symbolicate "%" name)))
    (multiple-value-bind (method-qualifiers method-args body) (process-operation-definition args)
      `(defmethod ,inner-name ,.method-qualifiers ((backend ,backend) ,@method-args) ,@body))))
