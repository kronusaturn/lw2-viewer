(uiop:define-package #:lw2.lmdb
  (:use #:cl #:sb-ext #:sb-thread #:alexandria #:iterate #:lw2.raw-memory-streams #:lw2.sites #:lw2.context #:lw2.backend-modules #:lw2-viewer.config #:lw2.hash-utils)
  (:export
   #:close-unused-environments
   #:define-cache-database #:with-cache-mutex #:with-cache-transaction #:with-cache-readonly-transaction #:with-db #:lmdb-put-string #:cache-put #:cache-get #:cache-del
   #:count-database-entries #:truncate-database
   #:call-with-cursor #:cursor-get
   #:existence
   #:binary-stream
   #:simple-cacheable #:define-lmdb-memoized #:*memoized-output-stream* #:*memoized-output-without-hyphens*)
  (:unintern #:lmdb-clear-db #:*db-mutex* #:*cache-environment-databases-list*))

(in-package #:lw2.lmdb) 

(defglobal *cache-databases-epoch* 0)

(defglobal *db-environments-lock* (make-mutex :name "DB environments mutex"))

(defglobal *db-environments* nil)

(defglobal *environments-sites* nil)

(defun define-cache-database (class-name &rest names)
  (with-mutex (*db-environments-lock*)
    (let* ((class (find-class class-name))
	   (old-list (class-own-databases class))
	   (new-list (union old-list names :test #'string= :key (lambda (x) (if (atom x) x (first x))))))
      (unless (equal old-list new-list)
	(incf *cache-databases-epoch*)
	(setf (class-own-databases class) new-list)))))

(defmethod class-databases ((class t)) nil)

(defmethod class-databases ((class backend-class))
  (if (eq *cache-databases-epoch* (class-databases-epoch class))
      (class-cached-databases class)
      (let ((new-list (append (class-own-databases class)
			      (loop for superclass in (closer-mop:class-direct-superclasses class)
				 append (class-databases superclass)))))
	(setf (class-cached-databases class) new-list
	      (class-databases-epoch class) *cache-databases-epoch*)
	new-list)))

(defun backend-databases (backend)
  (class-databases (class-of backend)))

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

(defun prepare-environment (environment-container backend)
  (let ((environment (environment-container-environment environment-container))
        (open-databases (environment-container-open-databases environment-container)))
    (assert (not lmdb:*transaction*) () "The transaction in which a database is created must be closed before that database may be used in another thread.")
    (with-environment-transaction (environment)
      (dolist (db-args (backend-databases backend))
	(destructuring-bind (db-name &key (flags 0)) (ensure-list db-args)
	  (unless (gethash db-name open-databases)
	    (let ((db (lmdb:make-database db-name :flags flags)))
	      (lmdb:open-database db :create t)
	      (setf (gethash db-name open-databases) db))))))
    (setf (environment-container-databases-list environment-container) (backend-databases backend))))

(defun find-environment-with-path (path environment-list)
  (find-if
   (lambda (env) (string= path (lmdb:environment-directory (environment-container-environment env))))
   environment-list))

(defun find-site-with-environment-path (path site-list)
  (find path site-list
	:test #'string=
	:key (lambda (site) (backend-cache-db-path (site-backend site)))))

(defun close-unused-environments ()
  (with-mutex (*db-environments-lock*)
    (let ((old-environments *db-environments*))
      (setf *db-environments* nil)
      (dolist (env old-environments)
	(if (find-site-with-environment-path (lmdb:environment-directory (environment-container-environment env)) *sites*)
	    (push env *db-environments*)
	    (progn
	      (wait-on-semaphore (environment-container-semaphore env) :n (expt 2 20))
	      (close-environment (environment-container-environment env) (environment-container-open-databases env))))))))

(define-backend-function get-current-environment ())

(define-backend-operation get-current-environment backend-lmdb-cache ()
  (with-mutex (*db-environments-lock*)
    (unless (and (backend-lmdb-environment backend) (eq *sites* *environments-sites*)
		 (eq (backend-databases backend) (environment-container-databases-list (backend-lmdb-environment backend))))
      (setf *environments-sites* *sites*)
      (let ((lmdb-cache-sites (remove-if (lambda (x) (not (typep (site-backend x) 'backend-lmdb-cache)))
					 *environments-sites*)))
	(uiop:ensure-all-directories-exist (map 'list
						(lambda (site)
						  (backend-cache-db-path (site-backend site)))
						lmdb-cache-sites))
	(dolist (site lmdb-cache-sites)
	  (if-let (existing-environment (find-environment-with-path (backend-cache-db-path (site-backend site)) *db-environments*))
		  (progn
		    (setf (backend-lmdb-environment (site-backend site)) existing-environment)
		    (prepare-environment existing-environment (site-backend site)))
		  (let ((new-environment
			 (make-environment-container
			  :semaphore (make-semaphore :name (format nil "LMDB environment semaphore for ~A" (site-host site)) :count (expt 2 20))
			  :environment (lmdb:make-environment (backend-cache-db-path (site-backend site))
							      :max-databases 1024 :max-readers 126 :open-flags 0 :mapsize *lmdb-mapsize*))))
		    (lmdb:open-environment (environment-container-environment new-environment) :create t)
		    (prepare-environment new-environment (site-backend site))
		    (setf (backend-lmdb-environment (site-backend site)) new-environment)
		    (push new-environment *db-environments*)))))))
  (backend-lmdb-environment backend))

(uiop:chdir (asdf:system-source-directory "lw2-viewer"))

(defun get-open-database (db-name)
  (let ((env (get-current-environment)))
    (with-mutex (*db-environments-lock*)
      (unless (eq (backend-databases *current-backend*) (environment-container-databases-list env))
	(prepare-environment env *current-backend*))
      (or (gethash db-name (environment-container-open-databases env))
	  (error "The database '~A' is not defined." db-name)))))

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

(defun cache-get (db-name key &key (return-type :string))
  (with-db (db db-name :read-only t)
	   (lmdb:get db (string-to-octets key :external-format :utf-8) :return-type return-type)))

(defun cache-del (db-name key &optional data)
  (with-db (db db-name)
    (lmdb:del db
	      (string-to-octets key :external-format :utf-8)
	      (and data (string-to-octets data :external-format :utf-8)))))

(defun count-database-entries (db-name)
  (with-db (db db-name :read-only t)
    (getf (lmdb:database-statistics db)
	  :entries)))

(defun truncate-database (db-name)
  (with-db (db db-name)
    (lmdb:drop-database db :delete 0)))

(defun call-with-cursor (db-name fn &key read-only)
  (with-db (db db-name :read-only read-only)
    (let ((cursor (lmdb:make-cursor db)))
      (lmdb:with-cursor (cursor)
	(funcall fn db cursor)))))

(defun cursor-get (cursor operation &optional key value (return-type :string))
  (lmdb:cursor-get cursor operation key value :return-type return-type))

(defun truncate-database-nicely (db-name)
  (iter
   (until
     (call-with-cursor
      db-name
      (lambda (db cursor)
	(declare (ignore db))
	(iter
	 (for limit from 100 above 0)
	 (if (cursor-get cursor :first nil nil 'existence)
	     (lmdb:cursor-del cursor)
	     (leave t))))))))

(defun existence (array size)
  (declare (ignore array size))
  t)

(defun binary-stream (array size)
  (make-instance 'raw-memory-stream :pointer array :length size))

(defun make-simple-cache (cache-db)
  (lambda (key value) (cache-put cache-db key value))) 

(defun wrap-handler (fn)
  (lambda (key)
    (handler-case
      (funcall fn key)
      (t () "[Error communicating with LW2 server]"))))

(defun make-simple-get (cache-db cache-fn get-real-fn get-wrapper-fn)
  (lambda (key)
    (labels ((inner (key)
	       (let ((val (cache-get cache-db key)))
		 (if val val
		     (let ((data (funcall get-real-fn key)))
		       (assert data)
		       (funcall cache-fn key data))))))
      (if get-wrapper-fn
	  (funcall get-wrapper-fn key #'inner)
	  (inner key)))))

(defmacro simple-cacheable ((base-name class-name cache-db key &key (catch-errors t) get-wrapper) &body body)
  (let ((get-real (intern (format nil "~:@(get-~A-real~)" base-name)))
	(cache (intern (format nil "~:@(cache-~A~)" base-name)))
	(get (intern (format nil "~:@(get-~A~)" base-name))))
    `(progn
       (define-cache-database ,class-name ,cache-db)
       (declaim (ftype (function (string) string) ,get-real ,get)
		(ftype (function (string string) string) ,cache))
       (setf (fdefinition (quote ,get-real)) (lambda (,key) ,@body)
	     (fdefinition (quote ,cache)) (make-simple-cache ,cache-db)
	     (fdefinition (quote ,get)) (,(if catch-errors 'wrap-handler 'identity)
					  (make-simple-get ,cache-db (fdefinition (quote ,cache)) (fdefinition (quote ,get-real)) ,get-wrapper)))))) 

(defvar *memoized-output-stream*)
(defvar *memoized-output-without-hyphens*) ;todo there's probably a better way to do this...

(defun write-memoized-data (array size)
  ;; This is unsafe anyway thanks to mem-aref, and it's pretty speed-critical
  (declare (optimize (safety 0) (debug 0)))
  (let ((out-stream *memoized-output-stream*))
    (if *memoized-output-without-hyphens*
	;; Filter out soft hyphens while writing to the output stream.
	;; Thanks to UTF-8's prefix-free property, we don't need to decode characters to
	;; do this, just search for the soft-hyphen byte sequence.
	(let ((hyphen-bytes (load-time-value (string-to-octets (string #\SOFT_HYPHEN) :external-format :utf-8)))
	      (hi 0))
	  (declare (type fixnum hi))
	  (loop for i from 0 to (1- size)
	     do (let ((in-byte (cffi:mem-aref array :unsigned-char i)))
		  (if (= in-byte (aref hyphen-bytes hi))
		      (if (= (1+ hi) (length hyphen-bytes))
			  (setf hi 0)
			  (incf hi))
		      (progn
			(when (/= hi 0)
			  (write-sequence hyphen-bytes out-stream :end hi)
			  (setf hi 0))
			(write-byte in-byte out-stream))))))
	;; In this case, keep the soft hyphens.
	(loop for i from 0 to (1- size)
	   do (write-byte (cffi:mem-aref array :unsigned-char i)
			  out-stream))))
  t)

(defun make-lmdb-memoized-wrapper (db-name fn return-type)
  (lambda (&rest args)
    (let* ((hash (hash-printable-object args))
           (cached-value (with-db (db db-name :read-only t) (lmdb:get db hash :return-type return-type))))
      (if cached-value
          cached-value
          (let* ((new-value (apply fn args))
                 (octets-value (string-to-octets new-value :external-format :utf-8)))
            (with-db (db db-name) (lmdb:put db hash octets-value))
            (case return-type
	      (:string new-value)
	      (:byte-vector octets-value)
	      ('write-memoized-data (with-db (db db-name :read-only t) (lmdb:get db hash :return-type return-type)))))))))

(defun clean-memoized-database (db-name class-name now-hash)
  (dolist (site *sites*)
    (let ((*current-backend* (site-backend site)))
      (when (and (typep *current-backend* 'backend-lmdb-cache)
		 (typep *current-backend* class-name))
	(unless (equalp now-hash (cache-get db-name "version" :return-type :byte-vector))
	  (cache-del db-name "version")
	  (truncate-database-nicely db-name)
	  (with-cache-transaction
	    (truncate-database db-name)
	    (with-db (db db-name) (lmdb:put db (string-to-octets "version" :external-format :utf-8) now-hash))))))))

(defmacro define-lmdb-memoized (name class-name (&key sources) lambda &body body)
  (let ((db-name (concatenate 'string (string-downcase (symbol-name name)) "-memo"))
	(alt-name (intern (format nil "~A*" name)))
        (now-hash (hash-file-list (list* "src/hash-utils.lisp" sources))))
    (alexandria:once-only (db-name now-hash)
			  `(progn
                             (define-cache-database ,class-name ,db-name)
			     (clean-memoized-database ,db-name ,class-name ,now-hash)
                             (declaim (ftype (function * string) ,name)
                                      (ftype (function * (values &optional t)) ,alt-name))
                             (let ((real-fn (lambda ,lambda ,@body)))
                               (setf (fdefinition (quote ,name)) (make-lmdb-memoized-wrapper ,db-name real-fn :string)
                                     (fdefinition (quote ,alt-name)) (make-lmdb-memoized-wrapper ,db-name real-fn 'write-memoized-data)))))))
