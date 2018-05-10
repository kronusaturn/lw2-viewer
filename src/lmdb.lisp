(uiop:define-package #:lw2.lmdb
  (:use #:cl #:sb-ext #:sb-thread #:alexandria #:lw2-viewer.config #:lw2.hash-utils)
  (:export #:with-cache-mutex #:with-cache-transaction #:with-db #:lmdb-put-string #:cache-put #:cache-get #:simple-cacheable #:define-lmdb-memoized)
  (:unintern #:lmdb-clear-db #:*db-mutex*))

(in-package #:lw2.lmdb) 

(defglobal *db-environment* nil)

(uiop:chdir (asdf:system-source-directory "lw2-viewer"))
(uiop:ensure-all-directories-exist (list *cache-db*))

(when (not *db-environment*)
  (setf *db-environment* (lmdb:make-environment *cache-db* :max-databases 1024 :max-readers 126 :open-flags 0 :mapsize *lmdb-mapsize*))
  (lmdb:open-environment *db-environment* :create t))

(defun call-with-cache-transaction (fn &key read-only)
  (if lmdb:*transaction*
      (funcall fn)
      (let ((txn (lmdb:make-transaction *db-environment* :flags (if read-only liblmdb:+rdonly+ 0))))
        (unwind-protect
          (progn
            (lmdb:begin-transaction txn)
            (let ((lmdb:*transaction* txn))
              (multiple-value-prog1
                (funcall fn)
                (lmdb:commit-transaction txn)
                (setf txn nil))))
          (when txn (lmdb:abort-transaction txn))))))

(defmacro with-cache-transaction (&body body)
  `(call-with-cache-transaction (lambda () ,@body)))

(defglobal *open-databases* (make-hash-table :test 'equal :synchronized t))

(defun get-open-database (db-name)
  (if-let (db (gethash db-name *open-databases*))
          db
          (with-locked-hash-table (*open-databases*)
            (with-cache-transaction
              (let ((db (lmdb:make-database db-name)))
                (lmdb:open-database db :create t)
                (setf (gethash db-name *open-databases*) db))))))

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
			     (unless (equalp ,now-hash (with-db (db ,db-name) (lmdb:get db ,version-octets)))
			       (with-db (db ,db-name)
					(lmdb:drop-database db :delete 0)
					(lmdb:put db ,version-octets ,now-hash)))
                             (declaim (ftype (function * string) ,name)
                                      (ftype (function * (vector (unsigned-byte 8))) ,alt-name))
                             (let ((real-fn (lambda ,lambda ,@body)))
                               (setf (fdefinition (quote ,name)) (make-lmdb-memoized-wrapper ,db-name real-fn :string)
                                     (fdefinition (quote ,alt-name)) (make-lmdb-memoized-wrapper ,db-name real-fn :byte-vector)))))))

