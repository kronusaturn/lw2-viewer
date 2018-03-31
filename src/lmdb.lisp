(uiop:define-package #:lw2.lmdb
  (:use #:cl #:sb-thread #:lw2-viewer.config)
  (:import-from #:flexi-streams #:string-to-octets #:octets-to-string) 
  (:export #:with-cache-mutex #:with-db #:lmdb-put-string #:cache-put #:cache-get #:simple-cacheable #:define-lmdb-memoized)
  (:unintern #:lmdb-clear-db))

(in-package #:lw2.lmdb) 

(defvar *db-mutex* (sb-thread:make-mutex :name "lmdb"))
(defvar *db-environment*)

(uiop:chdir (asdf:system-source-directory "lw2-viewer"))
(uiop:ensure-all-directories-exist (list *cache-db*))

(when (not (boundp '*db-environment*))
  (setq *db-environment* (lmdb:make-environment *cache-db* :max-databases 1024 :mapsize *lmdb-mapsize*))
  (lmdb:open-environment *db-environment* :create t))

(defmacro with-cache-mutex (&body body)
  `(with-mutex (*db-mutex*)
	       ,@body)) 

(defmacro with-db ((db db-name) &body body)
  (alexandria:with-gensyms (txn)
			   `(with-mutex (*db-mutex*)
					(let ((,txn (lmdb:make-transaction *db-environment* :flags 0))
					      (,db (lmdb:make-database ,db-name)))
					  (unwind-protect
					    (progn 
					      (lmdb:begin-transaction ,txn)
					      (let ((lmdb:*transaction* ,txn))
						(unwind-protect
						  (progn 
						    (lmdb:open-database ,db :create t) 
						    (prog1
						      (progn
							,@body)
						      (lmdb:commit-transaction ,txn)
						      (setf ,txn nil)))
						  (lmdb:close-database ,db)))) 
					    (when ,txn (lmdb:abort-transaction ,txn)))))))

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
  (with-db (db db-name) 
	   (let ((result (lmdb:get db (string-to-octets key :external-format :utf-8)))) 
	     (if result
	       (octets-to-string result :external-format :utf-8)
	       nil))))

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
       (declaim (ftype (function (string) string) ,get-real ,get)
		(ftype (function (string string) string) ,cache))
       (setf (fdefinition (quote ,get-real)) (lambda (,key) ,@body)
	     (fdefinition (quote ,cache)) (make-simple-cache ,cache-db)
	     (fdefinition (quote ,get)) (,(if catch-errors 'wrap-handler 'identity) (make-simple-get ,cache-db (fdefinition (quote ,cache)) (fdefinition (quote ,get-real)))))))) 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun city-hash-128-vector (data)
    (apply #'concatenate
	   (cons 'vector (map 'list #'bit-smasher:int->octets
			      (multiple-value-list
				(city-hash:city-hash-128 data)))))) 

  (defun hash-printable-object (object)
    (city-hash-128-vector (string-to-octets (prin1-to-string object) :external-format :utf-8)))
  
  (defun hash-file-list (file-list)
    (city-hash-128-vector
      (apply #'concatenate
	     `((vector character)
	       ,@(map 'list (lambda (f) (uiop:read-file-string (uiop:subpathname (asdf:system-source-directory "lw2-viewer") f))) file-list))))))

(defmacro define-lmdb-memoized (name (&key sources) lambda &body body)
  (let ((db-name (concatenate 'string (string-downcase (symbol-name name)) "-memo"))
	(version-octets (string-to-octets "version" :external-format :utf-8))
	(now-hash (hash-file-list sources)))
    (alexandria:once-only (db-name version-octets now-hash)
			  `(progn
			     (unless (equalp ,now-hash (with-db (db ,db-name) (lmdb:get db ,version-octets)))
			       (with-db (db ,db-name)
					(lmdb:drop-database db :delete 0)
					(lmdb:put db ,version-octets ,now-hash)))
			     (defun ,name (&rest args)
			       (labels ((real-fn ,lambda ,@body))
				 (let* ((hash (hash-printable-object args))
					(cached-value (with-db (db ,db-name) (lmdb:get db hash))))
				   (if cached-value
				     (octets-to-string cached-value :external-format :utf-8)
				     (let ((new-value (apply #'real-fn args)))
				       (with-db (db ,db-name) (lmdb:put db hash (string-to-octets new-value :external-format :utf-8)))
				       new-value))))))))) 

