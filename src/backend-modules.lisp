(uiop:define-package #:lw2.backend-modules
  (:use #:cl)
  (:import-from #:alexandria #:symbolicate)
  (:export
    #:backend-class #:class-cached-databases #:class-own-databases #:class-databases-epoch
    #:backend-base
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

(defclass backend-lmdb-cache (backend-base)
  ((lmdb-environment :accessor backend-lmdb-environment :initform nil)
   (cache-db-path :accessor backend-cache-db-path :initarg :cache-db-path :type simple-string))
  (:metaclass backend-class))

(defclass backend-graphql (backend-lmdb-cache)
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
			    backend-shortform
			    backend-lw2-tags) ()
  (:metaclass backend-class))

(defclass backend-accordius (backend-lw2-legacy backend-lw2-modernized)
  ((rest-api-uri :accessor rest-api-uri :initarg :rest-api-uri :type simple-string))
  (:metaclass backend-class))

(defclass backend-arbital (backend-lmdb-cache) ()
  (:metaclass backend-class))

(defun make-backend (type-string &rest args)
  (apply #'make-instance (symbolicate "BACKEND-" (string-upcase type-string)) args))

(defun process-operation-definition (args)
  (let* ((latter-args (member-if #'listp args))
	 (method-qualifiers (ldiff args latter-args))
	 (method-args (first latter-args))
	 (body (rest latter-args)))
    (values method-qualifiers method-args body)))

(defun operation-name-and-lambda-list-translator (name)
  (labels ((setf-lambda-list (backend lambda-list)
	     `(,(first lambda-list) (backend ,backend) ,@(rest lambda-list)))
	   (ordinary-lambda-list (backend lambda-list)
	     `((backend ,backend) ,@lambda-list)))
    (trivia:match name
		  ((list 'setf (and (type symbol) bare-name))
		   (let* ((inner-bare-name (symbolicate "%" bare-name))
			  (inner-name (list 'setf inner-bare-name)))
		     (values inner-name
			     #'setf-lambda-list
			     `(defun ,name (set-value &rest args) (setf (apply #',inner-bare-name lw2.context:*current-backend* args) set-value))
			     bare-name
			     inner-bare-name)))
		  ((type symbol)
		   (let ((inner-name (symbolicate "%" name)))
		     (values inner-name
			     #'ordinary-lambda-list
			     `(defun ,name (&rest args) (apply #',inner-name lw2.context:*current-backend* args))
			     name
			     inner-name)))
		  (_
		   (error "Invalid function name: ~A" name)))))

(defun names-only-lambda-list (lambda-list)
  (map 'list (lambda (x) (if (atom x) x (first x))) lambda-list))

(defmacro define-backend-function (name lambda-list &rest operations)
  (multiple-value-bind (inner-name lambda-list-translator wrapper-defun bare-name inner-bare-name)
      (operation-name-and-lambda-list-translator name)
  (let ((lambda-list (names-only-lambda-list lambda-list))
	(method-definitions
	 (mapcar (lambda (op)
		   (destructuring-bind (backend &rest body) op
		     `(:method ,(funcall lambda-list-translator backend lambda-list) ,@body)))
		 operations)))
   `(progn
      (export '(,bare-name ,inner-bare-name))
      (defgeneric ,inner-name ,(names-only-lambda-list (funcall lambda-list-translator t lambda-list)) ,.method-definitions)
      (declaim (inline ,name))
      ,wrapper-defun))))

(defmacro define-backend-operation (name backend &rest args)
  (multiple-value-bind (inner-name lambda-list-translator) (operation-name-and-lambda-list-translator name)
    (multiple-value-bind (method-qualifiers method-args body) (process-operation-definition args)
      `(defmethod ,inner-name ,.method-qualifiers ,(funcall lambda-list-translator backend method-args) ,@body))))
