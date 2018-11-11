(uiop:define-package #:lw2.lmdb
  (:use #:cl #:sb-ext #:sb-thread #:alexandria #:lw2.sites #:lw2.context #:lw2.backend-modules #:lw2-viewer.config #:lw2.hash-utils)
  (:export
    #:define-cache-database #:with-cache-mutex #:with-cache-transaction #:with-cache-readonly-transaction #:with-db #:lmdb-put-string #:cache-put #:cache-get #:simple-cacheable #:define-lmdb-memoized)
  (:unintern #:lmdb-clear-db #:*db-mutex* #:*cache-environment-databases-list*))

(in-package #:lw2.lmdb) 

(defglobal *cache-databases-list* nil)

(defglobal *db-environments-lock* (make-mutex :name "DB environments mutex"))

(defglobal *db-environments* nil)

(defglobal *environments-sites* nil)

(defun define-cache-database (&rest names)
  (with-mutex (*db-environments-lock*)
    (dolist (name names)
      (setf *cache-databases-list* (adjoin name *cache-databases-list* :test #'string=)))))

(defstruct environment-container
  (semaphore nil :type semaphore)
  (environment nil :type lmdb:environment)
  (open-databases (make-hash-table :test 'equal) :type hash-table)
  (databases-list nil))

(defun call-with-environment-transaction (fn environment &key read-only)
  (if lmdb:*transaction*
      (funcall fn)
      (let ((txn (lmdb:make-transaction environment :flags (if read-only liblmdb:+rdonly+ 0))))
        (unwind-protect
          (progn
            (lmdb:begin-transaction txn)
            (let ((lmdb:*transaction* txn))
              (multiple-value-prog1
                (funcall fn)
                (lmdb:commit-transaction txn)
                (setf txn nil))))
          (when txn (lmdb:abort-transaction txn))))))

(defmacro with-environment-transaction ((environment) &body body)
  `(call-with-environment-transaction (lambda () ,@body) ,environment))

(defun close-environment (environment open-databases)
  (with-environment-transaction (environment)
    (maphash (lambda (k v)
               (declare (ignore k))
               (lmdb:close-database v :transaction lmdb:*transaction*))
             open-databases))
  (lmdb:close-environment environment))

(defun prepare-environment (environment-container)
  (let ((environment (environment-container-environment environment-container))
        (open-databases (environment-container-open-databases environment-container)))
    (with-environment-transaction (environment)
      (dolist (db-name *cache-databases-list*)
        (let ((db (lmdb:make-database db-name)))
          (lmdb:open-database db :create t)
          (setf (gethash db-name open-databases) db))))
    (setf (environment-container-databases-list environment-container) *cache-databases-list*)))

(define-backend-function get-current-environment ())

(define-backend-operation get-current-environment backend-lmdb-cache ()
  (with-mutex (*db-environments-lock*)
    (unless (and (backend-lmdb-environment backend) (eq *sites* *environments-sites*))
      (dolist (env *db-environments*)
        (wait-on-semaphore (environment-container-semaphore env) :n (expt 2 20))
        (close-environment (environment-container-environment env) (environment-container-open-databases env)))
      (setf *db-environments* nil
            *environments-sites* *sites*)
      (uiop:ensure-all-directories-exist (map 'list
                                              (lambda (site)
                                                (backend-cache-db-path (site-backend site)))
                                              *environments-sites*))
      (dolist (site *environments-sites*)
        (let ((new-environment
                (make-environment-container
                  :semaphore (make-semaphore :name (format nil "LMDB environment semaphore for ~A" (site-host site)) :count (expt 2 20))
                  :environment (lmdb:make-environment (backend-cache-db-path (site-backend site)) :max-databases 1024 :max-readers 126 :open-flags 0 :mapsize *lmdb-mapsize*))))
          (lmdb:open-environment (environment-container-environment new-environment) :create t)
          (prepare-environment new-environment)
          (setf (backend-lmdb-environment (site-backend site)) new-environment)
          (push new-environment *db-environments*))))
    (backend-lmdb-environment backend)))

(uiop:chdir (asdf:system-source-directory "lw2-viewer"))

(defun get-open-database (db-name)
  (let ((env (get-current-environment)))
    (with-mutex (*db-environments-lock*)
      (unless (eq *cache-databases-list* (environment-container-databases-list env))
        (prepare-environment env)))
    (or (gethash db-name (environment-container-open-databases env))
        (error "The database '~A' is not defined." db-name))))

(defun call-with-cache-transaction (fn &key read-only)
  (let ((env (get-current-environment)))
    (unwind-protect
      (progn
        (wait-on-semaphore (environment-container-semaphore env))
        (call-with-environment-transaction fn (environment-container-environment env) :read-only read-only))
      (signal-semaphore (environment-container-semaphore env)))))

(defmacro with-cache-transaction (&body body)
  `(call-with-cache-transaction (lambda () ,@body)))

(defmacro with-cache-readonly-transaction (&body body)
  `(call-with-cache-transaction (lambda () ,@body) :read-only t))

(defmacro with-db ((db db-name &key read-only) &body body)
  `(let ((,db (get-open-database ,db-name)))
     (call-with-cache-transaction
       (lambda () ,@body)
       :read-only ,read-only)))

(defun lmdb-put-string (db key value)
  (if
    (lmdb:put db
	      (string-to-octets key :external-format :utf-8)
	      (string-to-octets value :external-format :utf-8))
    value
    nil))

(defun cache-put (db-name key value)
  (with-db (db db-name) 
	   (lmdb-put-string db key value)))

(defun cache-get (db-name key)
  (with-db (db db-name :read-only t)
	   (lmdb:get db (string-to-octets key :external-format :utf-8) :return-type :string)))

(defun make-simple-cache (cache-db)
  (lambda (key value) (cache-put cache-db key value))) 

(defun wrap-handler (fn)
  (lambda (key)
    (handler-case
      (funcall fn key)
      (t () "[Error communicating with LW2 server]"))))

(defun make-simple-get (cache-db cache-fn get-real-fn)
  (lambda (key) 
    (let ((val (cache-get cache-db key)))
      (if val val
	(let ((data (funcall get-real-fn key)))
	  (assert data)
	  (funcall cache-fn key data)))))) 

(defmacro simple-cacheable ((base-name cache-db key &key (catch-errors t)) &body body)
  (let ((get-real (intern (format nil "~:@(get-~A-real~)" base-name)))
	(cache (intern (format nil "~:@(cache-~A~)" base-name)))
	(get (intern (format nil "~:@(get-~A~)" base-name))))
    `(progn
       (define-cache-database ,cache-db)
       (declaim (ftype (function (string) string) ,get-real ,get)
		(ftype (function (string string) string) ,cache))
       (setf (fdefinition (quote ,get-real)) (lambda (,key) ,@body)
	     (fdefinition (quote ,cache)) (make-simple-cache ,cache-db)
	     (fdefinition (quote ,get)) (,(if catch-errors 'wrap-handler 'identity) (make-simple-get ,cache-db (fdefinition (quote ,cache)) (fdefinition (quote ,get-real)))))))) 

(defun make-lmdb-memoized-wrapper (db-name fn return-type)
  (lambda (&rest args)
    (let* ((hash (hash-printable-object args))
           (cached-value (with-db (db db-name) (lmdb:get db hash :return-type return-type))))
      (if cached-value
          cached-value
          (let* ((new-value (apply fn args))
                 (octets-value (string-to-octets new-value :external-format :utf-8)))
            (with-db (db db-name) (lmdb:put db hash octets-value))
            (ecase return-type (:string new-value) (:byte-vector octets-value)))))))

(defmacro define-lmdb-memoized (name (&key sources) lambda &body body)
  (let ((db-name (concatenate 'string (string-downcase (symbol-name name)) "-memo"))
        (alt-name (intern (format nil "~A*" name)))
	(version-octets (string-to-octets "version" :external-format :utf-8))
        (now-hash (hash-file-list (list* "src/hash-utils.lisp" sources))))
    (alexandria:once-only (db-name version-octets now-hash)
			  `(progn
                             (define-cache-database ,db-name)
                             (dolist (site *sites*)
                               (let ((*current-backend* (site-backend site)))
                                 (when (typep *current-backend* 'backend-lmdb-cache)
                                   (unless (equalp ,now-hash (with-db (db ,db-name) (lmdb:get db ,version-octets)))
                                     (with-db (db ,db-name)
                                              (lmdb:drop-database db :delete 0)
                                              (lmdb:put db ,version-octets ,now-hash))))))
                             (declaim (ftype (function * string) ,name)
                                      (ftype (function * (vector (unsigned-byte 8))) ,alt-name))
                             (let ((real-fn (lambda ,lambda ,@body)))
                               (setf (fdefinition (quote ,name)) (make-lmdb-memoized-wrapper ,db-name real-fn :string)
                                     (fdefinition (quote ,alt-name)) (make-lmdb-memoized-wrapper ,db-name real-fn :byte-vector)))))))

