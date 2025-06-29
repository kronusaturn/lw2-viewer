(uiop:define-package #:lw2.lmdb
  (:use #:cl #:sb-ext #:sb-thread #:alexandria #:iterate #:lw2.raw-memory-streams #:lw2.conditions #:lw2.sites #:lw2.context #:lw2.backend-modules #:lw2-viewer.config #:lw2.hash-utils)
  (:import-from #:lw2.rwlock #:rwlock #:make-rwlock #:with-read-lock #:with-write-lock #:with-rwlock-protect)
  (:export
   #:close-unused-environments
   #:define-cache-database #:with-cache-mutex #:with-cache-transaction #:with-cache-readonly-transaction
   #:cache-put #:cache-get #:cache-exists #:cache-del
   #:count-database-entries #:truncate-database
   #:call-with-cursor #:cursor-get
   #:existence
   #:binary-stream
   #:simple-cacheable #:define-lmdb-memoized #:current-memo-hash #:*memoized-output-stream* #:*memoized-output-without-hyphens*)
  (:unintern #:lmdb-clear-db #:lmdb-put-string #:*db-mutex* #:*cache-environment-databases-list* #:*db-environments-lock*))

(in-package #:lw2.lmdb) 

(defglobal *cache-databases-epoch* 0)

(defglobal *db-environments-rwlock* (make-rwlock))

(defglobal *db-environments* nil)

(defglobal *environments-sites* nil)

(defun define-cache-database (class-name &rest names)
  (with-write-lock (*db-environments-rwlock*)
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
  (rwlock nil :type rwlock)
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
  (with-write-lock (*db-environments-rwlock*)
    (let ((old-environments *db-environments*))
      (setf *db-environments* nil)
      (dolist (env old-environments)
	(if (find-site-with-environment-path (lmdb:environment-directory (environment-container-environment env)) *sites*)
	    (push env *db-environments*)
	    (progn
	      (with-write-lock ((environment-container-rwlock env))
		(close-environment (environment-container-environment env) (environment-container-open-databases env)))))))))

(define-backend-function get-current-environment ())

(define-backend-operation get-current-environment backend-lmdb-cache ()
  (with-rwlock-protect *db-environments-rwlock*
    (and (backend-lmdb-environment backend) (eq *sites* *environments-sites*)
	 (eq (backend-databases backend) (environment-container-databases-list (backend-lmdb-environment backend))))
    (progn
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
		    :rwlock (make-rwlock)
		    :environment (lmdb:make-environment (backend-cache-db-path (site-backend site))
							:max-databases 1024 :max-readers 512 :open-flags 0 :mapsize *lmdb-mapsize*))))
	      (lmdb:open-environment (environment-container-environment new-environment) :create t)
	      (prepare-environment new-environment (site-backend site))
	      (setf (backend-lmdb-environment (site-backend site)) new-environment)
	      (push new-environment *db-environments*))))))
    (backend-lmdb-environment backend)))

(uiop:chdir (asdf:system-source-directory "lw2-viewer"))

(defun get-open-database (db-name)
  (let ((env (get-current-environment)))
    (with-read-lock (*db-environments-rwlock* :upgrade-fn upgrade-lock)
      (unless (eq (backend-databases *current-backend*) (environment-container-databases-list env))
	(upgrade-lock)
	(prepare-environment env *current-backend*))
      (or (gethash db-name (environment-container-open-databases env))
	  (error "The database '~A' is not defined." db-name)))))

(defun call-with-cache-transaction (fn &key read-only)
  (if lmdb:*transaction*
      (funcall fn)
      (let ((env (get-current-environment)))
	(with-read-lock ((environment-container-rwlock env))
	  (call-with-environment-transaction fn (environment-container-environment env) :read-only read-only)))))

(defmacro with-cache-transaction (&body body)
  `(call-with-cache-transaction (lambda () ,@body)))

(defmacro with-cache-readonly-transaction (&body body)
  `(call-with-cache-transaction (lambda () ,@body) :read-only t))

(defmacro with-db ((db db-name &key read-only) &body body)
  `(let ((,db (get-open-database ,db-name)))
     (call-with-cache-transaction
       (lambda () ,@body)
       :read-only ,read-only)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *type-accessors*
    '((:byte-vector (vector (unsigned-byte 8)) x x :byte-vector)
      (:uint64 (unsigned-byte 64)
       (let ((vec (nibbles:make-octet-vector 8))) (setf (nibbles:ub64ref/le vec 0) x) vec)
       (and x (nibbles:ub64ref/le x 0))
       :byte-vector)
      (:string string (and x (string-to-octets x :external-format :utf-8)) x :string)
      (:json t
       (and x (string-to-octets (json:encode-json-to-string x) :external-format :utf-8))
       (and x (json:decode-json (flex:make-flexi-stream x :external-format :utf-8))) 'binary-stream)
      (:lisp t
       (and x (string-to-octets (write-to-string x :pretty nil :readably t :circle nil) :external-format :utf-8))
       (and x (read (flex:make-flexi-stream x :external-format :utf-8))) 'binary-stream)))

  (defun map-type-accessors (fn)
    (map 'list (lambda (accessor) (apply fn accessor)) *type-accessors*))

  (defun type-accessor-fn (form)
    (coerce
     `(lambda (key type encoder decoder lmdb-type)
	(declare (ignorable key type encoder decoder lmdb-type))
	,form)
     'function)))

(defmacro type-accessor-case (type form)
  `(case ,type
     ,@(map-type-accessors (type-accessor-fn ``(,key ,,form)))))

(defun encode-value (x type)
  (type-accessor-case type encoder))

(defun accessor-decoder (return-type value-type)
  (if return-type (values return-type #'identity) (type-accessor-case value-type `(values ,lmdb-type (lambda (x) ,decoder)))))

(defun cache-put (db-name key value &key (key-type :string) (value-type :string))
  (with-db (db db-name)
    (if (lmdb:put db
		  (encode-value key key-type)
		  (encode-value value value-type))
	value
	nil)))

(defun cache-get (db-name key &key (key-type :string) (value-type :string) return-type)
  (multiple-value-bind (lmdb-type decoder) (accessor-decoder return-type value-type)
    (with-db (db db-name :read-only t)
      (funcall decoder (lmdb:get db (encode-value key key-type) :return-type lmdb-type)))))

(defun cache-exists (db-name key &key (key-type :string))
  (with-db (db db-name :read-only t)
    (lmdb:get db (encode-value key key-type) :return-type 'existence)))

(defun cache-del (db-name key &key value (key-type :string) (value-type :string))
  (with-db (db db-name)
    (lmdb:del db
	      (encode-value key key-type)
	      (and value (encode-value value value-type)))))

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

(defun cursor-get (cursor operation &key key value (key-type :string) (value-type :string) return-type)
  (multiple-value-bind (key-lmdb-type key-decoder) (accessor-decoder return-type key-type)
    (multiple-value-bind (value-lmdb-type value-decoder) (accessor-decoder return-type value-type)
      (multiple-value-bind (return-value return-key)
	  (lmdb:cursor-get cursor operation (encode-value key key-type) (encode-value value value-type)
			   :return-type value-lmdb-type :key-return-type key-lmdb-type)
	(values (funcall value-decoder return-value)
		(funcall key-decoder return-key))))))

(defun truncate-database-nicely (db-name)
  (iter
   (until
     (call-with-cursor
      db-name
      (lambda (db cursor)
	(declare (ignore db))
	(iter
	 (for limit from 100 above 0)
	 (if (cursor-get cursor :first :return-type 'existence)
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
(defparameter *memoized-output-dynamic-blocks* nil)

(defun write-memoized-data (array size)
  ;; This is unsafe anyway thanks to mem-aref, and it's pretty speed-critical
  (declare (optimize (safety 0) (debug 0))
	   (type (and fixnum (integer 0)) size)
	   (type cffi:foreign-pointer array))
  (let ((out-stream *memoized-output-stream*)
	(dynamic-blocks *memoized-output-dynamic-blocks*)
	(index 0)
	(buffer (make-array 2048 :element-type '(unsigned-byte 8) :initial-element 0))
	(buffer-index 0))
    (declare (dynamic-extent buffer)
	     (type (and fixnum (integer 0)) index)
	     (type (integer 0 2048) buffer-index))
    (labels ((flush-buffer ()
	       (when (> buffer-index 0)
		 (write-sequence buffer out-stream :end buffer-index)
		 (setf buffer-index 0)))
	     (output-byte (byte)
	       (when (= buffer-index 2048)
		 (flush-buffer))
	       (setf (aref buffer buffer-index) byte)
	       (incf buffer-index))
	     (process-span (start end)
	       (declare (type (and fixnum (integer 0)) start end))
	       (if *memoized-output-without-hyphens*
		   ;; Filter out soft hyphens while writing to the output stream.
		   ;; Thanks to UTF-8's prefix-free property, we don't need to decode characters to
		   ;; do this, just search for the soft-hyphen byte sequence.
		   (let ((hyphen-bytes (load-time-value (string-to-octets (string #\SOFT_HYPHEN) :external-format :utf-8)))
			 (hi 0))
		     (declare (type (unsigned-byte 3) hi))
		     (iter (for i from start below end)
			(let ((in-byte (cffi:mem-aref array :unsigned-char i)))
			  (if (= in-byte (aref hyphen-bytes hi))
			      (if (= (1+ hi) (length hyphen-bytes))
				  (setf hi 0)
				  (incf hi))
			      (progn
				(when (/= hi 0)
				  (iter (for i from 0 below hi) (output-byte (aref hyphen-bytes i)))
				  (setf hi 0))
				(output-byte in-byte))))))
		   ;; In this case, keep the soft hyphens.
		   (iter (for i from start below end)
			 (output-byte (cffi:mem-aref array :unsigned-char i))))
	       (flush-buffer)))
      (declare (dynamic-extent #'output-byte #'flush-buffer #'process-span))
      (iter (for dynamic-block in dynamic-blocks)
	    (destructuring-bind (start end fn &rest args) dynamic-block
	      (process-span index start)
	      (setf index start)
	      (log-and-ignore-errors
	       (handler-case
		   (progn (apply fn args) (values))
		 (:no-error () (setf index end)))))
	    (finally
	     (process-span index size)))))
  t)

(declaim (ftype (function (cffi:foreign-pointer fixnum (simple-array (unsigned-byte 8) (*))) boolean) version-equal)
	 (inline version-equal))

(defun version-equal (array size version)
  (declare (type (unsigned-byte 32) size)
	   (type (simple-array (unsigned-byte 8) (*)) version))
  (if (>= size (length version))
      (loop for i from 0 below (length version)
	 do (when (/= (aref version i)
		      (cffi:mem-aref array :unsigned-char i))
	      (return nil))
	 finally
	   (return t))))

(defstruct scoreboard
  (table (make-hash-table :test 'equalp))
  (mutex (sb-thread:make-mutex)))

(defstruct scorecard
  (ready nil)
  (mutex (sb-thread:make-mutex))
  (waitqueue (sb-thread:make-waitqueue)))

(defun call-with-scorecard (scoreboard key created-fn existing-fn)
  (declare (type (function ()) created-fn existing-fn))
  (let ((table (scoreboard-table scoreboard))
	scorecard
	created)
    (sb-thread:with-mutex ((scoreboard-mutex scoreboard))
      (setf scorecard (gethash key table))
      (unless scorecard
	(setf created t
	      scorecard (make-scorecard)
	      (gethash key table) scorecard)))
    (cond
      (created
       (unwind-protect
	    (funcall created-fn)
	 (setf (scorecard-ready scorecard) t)
	 (sb-thread:with-mutex ((scorecard-mutex scorecard))
	   (sb-thread:condition-broadcast (scorecard-waitqueue scorecard)))
	 (sb-thread:with-mutex ((scoreboard-mutex scoreboard))
	   (remhash key table)))
       (funcall existing-fn))
      (:otherwise
	 (unless (scorecard-ready scorecard)
	   (sb-thread:with-mutex ((scorecard-mutex scorecard))
	     (loop until (scorecard-ready scorecard)
		do (or (sb-thread:condition-wait (scorecard-waitqueue scorecard) (scorecard-mutex scorecard))
		       (error "Waitqueue error.")))))
	 (funcall existing-fn)))))

(defmacro scorecard-protect (scoreboard key when-not-ready when-ready)
  "
Protect multiple threads from simultaneous execution. When multiple threads
evaluate SCORECARD-PROTECT with the same SCOREBOARD and KEYs that are EQUALP,
only the first thread evaluates WHEN-NOT-READY, and other threads wait for it
to finish. Finally, all threads evaluate WHEN-READY, and its values are returned."
  `(flet ((when-not-ready () ,when-not-ready)
	  (when-ready () ,when-ready))
     (declare (dynamic-extent #'when-not-ready #'when-ready))
     (call-with-scorecard ,scoreboard ,key #'when-not-ready #'when-ready)))

(defun make-lmdb-memoized-wrapper (db-name version scoreboard fn return-type)
  (declare (type (simple-array (unsigned-byte 8) (*)) version))
  (labels ((unchecked-return-type (array size)
	     (declare (type (unsigned-byte 32) size))
	     (let* ((version-size (length version))
		    (data-size (- size version-size)))
	       (ecase return-type
		 (:string (cffi:foreign-string-to-lisp array :offset (length version) :count data-size))
		 ('write-memoized-data (write-memoized-data (cffi:inc-pointer array version-size) data-size)))))
	   (cached-return-type (array size)
	     (when (version-equal array size version)
	       (unchecked-return-type array size))))
    (lambda (&rest args)
      (let* ((hash (hash-printable-object args))
	     (*memoized-output-dynamic-blocks* (cache-get "dynamic-content-blocks" hash :key-type :byte-vector :value-type :lisp))
	     (cached-value (cache-get db-name hash :key-type :byte-vector :return-type #'cached-return-type)))
	(if cached-value
	    cached-value
	    (scorecard-protect scoreboard hash
	      (let* ((new-value (apply fn hash args))
		     (octets-value (concatenate '(simple-array (unsigned-byte 8) (*))
						version
						(string-to-octets new-value :external-format :utf-8))))
		(with-cache-transaction
		  (cache-put db-name hash octets-value :key-type :byte-vector :value-type :byte-vector)
		  (setf *memoized-output-dynamic-blocks* (cache-get "dynamic-content-blocks" hash :key-type :byte-vector :value-type :lisp))))
	      (cache-get db-name hash :key-type :byte-vector :return-type #'unchecked-return-type)))))))

(defmacro define-lmdb-memoized (name class-name (&key sources) lambda &body body)
  (let ((db-name (concatenate 'string (string-downcase (symbol-name name)) "-memo"))
	(alt-name (intern (format nil "~A*" name)))
	(scoreboard-name (intern (format nil "*~A-SCOREBOARD*" name)))
	(now-hash (hash-file-list (list* "src/hash-utils.lisp" sources))))
    (alexandria:once-only (db-name now-hash)
			  `(progn
			     (define-cache-database ,class-name ,db-name)
			     (declaim (ftype (function * string) ,name)
				      (ftype (function * (values &optional t)) ,alt-name))
			     (eval-when (:compile-toplevel :load-toplevel :execute)
			       (defglobal ,scoreboard-name (make-scoreboard)))
			     (let ((real-fn (lambda (current-memo-hash ,@lambda)
					      (declare (ignorable current-memo-hash))
					      ,@body)))
			       (setf (fdefinition (quote ,name)) (make-lmdb-memoized-wrapper ,db-name ,now-hash ,scoreboard-name real-fn :string)
				     (fdefinition (quote ,alt-name)) (make-lmdb-memoized-wrapper ,db-name ,now-hash ,scoreboard-name real-fn 'write-memoized-data)))))))
