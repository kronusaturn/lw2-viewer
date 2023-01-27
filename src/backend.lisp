(uiop:define-package #:lw2.backend
  (:use #:cl #:sb-thread #:flexi-streams #:alexandria #:iterate #:lw2-viewer.config #:lw2.sites #:lw2.context #:lw2.graphql #:lw2.lmdb
	#:lw2.utils #:lw2.hash-utils #:lw2.backend-modules #:lw2.schema-type #:lw2.conditions #:lw2.web-push)
  (:import-from #:collectors #:with-collector)
  (:import-from #:lw2.user-context #:*current-auth-token*)
  (:reexport #:lw2.backend-modules)
  (:export #:*use-alignment-forum*
	   #:*graphql-debug-output*
	   #:*revalidate-default* #:*force-revalidate-default*
           #:*messages-index-fields*
           #:*notifications-base-terms*
           #:start-background-loader #:stop-background-loader #:background-loader-running-p
	   #:call-with-http-response
	   #:forwarded-header #:backend-request-headers
	   #:lw2-graphql-query #:lw2-query-string* #:lw2-query-string
           #:lw2-graphql-query-map #:lw2-graphql-query-multi
	   #:signal-lw2-errors
	   #:earliest-post-time
	   #:flatten-shortform-comments #:get-shortform-votes
	   #:get-tag-posts
	   #:get-post-tag-votes #:get-tag-post-votes
	   #:get-slug-tagid
	   #:get-posts-index #:get-posts-json #:get-post-body #:get-post-vote #:get-post-comments #:get-post-answers
	   #:get-post-comments-votes
	   #:get-tag-comments-votes
	   #:get-recent-comments #:get-recent-comments-json
	   #:sequence-post-ids #:get-sequence #:get-post-sequence-ids #:get-sequence-post
	   #:get-conversation-messages
	   #:markdown-source
	   #:get-user
           #:get-notifications #:check-notifications
	   #:mark-comment-replied #:check-comment-replied
	   #:lw2-search-query #:get-post-title #:get-post-slug #:get-slug-postid #:get-username #:get-user-full-name #:get-user-slug
	   #:do-wl-rest-mutate #:do-wl-rest-query #:do-wl-create-tag)
  (:recycle #:lw2-viewer)
  (:unintern #:get-posts #:make-posts-list-query #:define-backend-fields
	     #:*posts-index-fields* #:posts-index-fields #:post-body-fields
	     #:*comments-index-fields* #:comments-index-fields
	     #:*post-comments-fields* #:post-comments-fields
	     #:define-index-fields #:decode-graphql-json
	     #:lw2-graphql-query-noparse #:lw2-graphql-query-streamparse
	     #:*cookie-jar*
	     #:with-connection-pool #:call-with-connection-pool))

(in-package #:lw2.backend)

;; Dexador settings required for the system to work properly.
(setf dex:*default-connect-timeout* nil
      dex:*default-read-timeout* nil
      dex:*use-connection-pool* nil)

(defvar *use-alignment-forum* nil)

(defvar *graphql-debug-output* nil)

(defvar *revalidate-default* t)
(defvar *force-revalidate-default* nil)

(defparameter *messages-index-fields* '(:--id :user-id :created-at (:contents :html) (:conversation :--id :title) :----typename))
(defparameter *user-fields* '(:--id :slug :display-name :karma))

(defparameter *notifications-base-terms* (alist :view "userNotifications" :created-at :null :viewed :null))

(defun request-fields (query-type return-type context)
  "Returns the desired fields for a given type of request."
  (case return-type
    (:total nil)
    (t
     (with-collector (col)
       (let ((backend *current-backend*)
	     (schema-type (find-schema-type query-type))
	     (added (make-hash-table :test 'eq)))
	 (dolist (field (cdr (assoc :fields schema-type)) (col))
	   (destructuring-bind (field-name field-type &key alias backend-type graphql-ignore subfields ((:context field-context)) context-not &allow-other-keys) field
	     (declare (ignore field-type))
	     (when (and (not (gethash field-name added))
			(not graphql-ignore)
			(or (not backend-type) (typep backend backend-type))
			(or (not field-context) (eq context field-context))
			(or (not context-not) (not (eq context context-not))))
	       (setf (gethash field-name added) t)
	       (col
		(let ((result-name (or alias field-name)))
		  (if subfields
		      (list* result-name subfields)
		      result-name)))))))))))

(define-backend-function user-fields ()
  (backend-lw2-legacy (load-time-value *user-fields*))
  (backend-lw2-modernized (append (call-next-method) '(:groups :deleted :html-bio)))
  (backend-alignment-forum (append (call-next-method) '(:af-karma :full-name))))

(define-cache-database 'backend-lw2-legacy
    "index-json"
    "post-comments-json" "post-comments-json-meta" "post-answers-json" "post-answers-json-meta"
    "post-body-json" "post-body-json-meta"
    "sequence-json" "sequence-json-meta" "post-sequence"
    "user-json" "user-json-meta"
    "user-page-items" "user-page-items-meta")

(define-cache-database 'backend-lw2-modernized
    "user-deleted")

(define-backend-function comments-list-to-graphql-json (comments-list)
  (backend-lw2-legacy
   (json:encode-json-to-string
    (plist-hash-table (list :data (plist-hash-table (list :*comments-list comments-list))))))
  (backend-lw2-modernized
   (json:encode-json-to-string
    (plist-hash-table (list :data (plist-hash-table (list :*comments-list (plist-hash-table (list :results comments-list)))))))))

(defun do-graphql-debug (query)
  (when *graphql-debug-output*
    (format *graphql-debug-output* "~&GraphQL query: ~A~%" query)))

(defmacro with-retrying ((maybe-retry-fn-name &key retries before-maybe-retry before-retry) &body body)
  (with-gensyms (remaining-retries retry)
    `(let ((,remaining-retries ,retries))
       (tagbody ,retry
	  (labels ((,maybe-retry-fn-name ()
		     ,before-maybe-retry
		     (when (> ,remaining-retries 0)
		       (decf ,remaining-retries)
		       ,before-retry
		       (go ,retry))))
	    ,@body)))))

(defun force-close (stream)
  (ignore-errors (close stream :abort t)))

(sb-ext:defglobal *connection-pool* (make-hash-table :test 'equal))
(sb-ext:defglobal *connection-pool-lock* (sb-thread:make-mutex :name "*connection-pool-lock*"))

(defun connection-push (dest connection)
  (let ((connection-pool *connection-pool*)
	old-connection)
    (sb-thread:with-mutex (*connection-pool-lock*)
      (let ((vector (or (gethash dest connection-pool)
			(setf (gethash dest connection-pool)
			      (make-array 4 :fill-pointer 0)))))
	(unless (vector-push connection vector)
	  (setf old-connection (vector-pop vector))
	  (vector-push connection vector))))
    (when old-connection
      (force-close old-connection))))

(defun connection-pop (dest)
  (let ((connection-pool *connection-pool*))
    (sb-thread:with-mutex (*connection-pool-lock*)
      (when-let (vector (gethash dest connection-pool))
		(when (> (fill-pointer vector) 0)
		  (vector-pop vector))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (token-bucket (:constructor %make-token-bucket))
    (tokens internal-time-units-per-second :type fixnum)
    (base-cost internal-time-units-per-second :type fixnum)
    (last-update (get-internal-real-time) :type fixnum)
    (tokens-per-unit 1 :type fixnum)
    (token-limit internal-time-units-per-second :type fixnum))

  (defun make-token-bucket (&key rate burst (fill-ratio 1.0))
    (let* ((scaled-rate (rationalize (/ internal-time-units-per-second rate)))
	   (base-cost (numerator scaled-rate))
	   (tokens-per-unit (denominator scaled-rate))
	   (token-limit (* base-cost burst)))
      (%make-token-bucket
       :base-cost base-cost
       :tokens-per-unit tokens-per-unit
       :token-limit token-limit
       :tokens (round (* token-limit fill-ratio))))))

(defun token-bucket-decrement (token-bucket n &optional with-punishment)
  (let* ((time (get-internal-real-time))
	 (tpu (token-bucket-tokens-per-unit token-bucket))
	 (limit (token-bucket-token-limit token-bucket))
	 (cost (round (* n (token-bucket-base-cost token-bucket))))
	 (max-increment (+ limit cost))
	 (max-timediff (ceiling max-increment tpu))
	 (increment 0)
	 (result nil))
    (declare (type (and fixnum (integer 0)) time)
	     (type (unsigned-byte 32) limit cost max-increment increment)
	     (type (integer 1 10000000) tpu))
    (sb-ext:atomic-update (token-bucket-last-update token-bucket)
			  (lambda (previous)
			    (declare (type (and fixnum (integer 0)) previous))
			    (if (> (- time max-timediff) previous)
				(setf increment max-increment)
				(let ((timediff (- time (min previous time))))
				  (declare (type (unsigned-byte 32) timediff))
				  (setf increment (* timediff tpu))))
			    (max previous time)))
    (sb-ext:atomic-update (token-bucket-tokens token-bucket)
			  (lambda (previous)
			    (declare (type (signed-byte 32) previous))
			    (let* ((new-refill (+ previous increment))
				   (new (- new-refill cost)))
			      (setf result (> new 0))
			      (if (or result with-punishment)
				  (min new limit)
				  (max (- limit) (min new-refill limit))))))
    result))

(defun parse-ipv4 (string)
  (let ((l (map 'list #'parse-integer
		(split-sequence:split-sequence #\. string))))
    (loop with res = 0
       for n in l
       do (setf res (+ n (ash res 8)))
       finally (return res))))

(defparameter *rate-limit-cost-factor* 1)

(sb-ext:defglobal *global-token-bucket* (make-token-bucket :rate 3 :burst 180))

(defun check-rate-limit ()
  (or (token-bucket-decrement *global-token-bucket* *rate-limit-cost-factor*)
      (error "Rate limit exceeded. Try again later.")))

(defun call-with-http-response (fn uri-string &rest args &key &allow-other-keys)
  (check-rate-limit)
  (let* ((uri (quri:uri uri-string))
	 (uri-dest (concatenate 'string (quri:uri-host uri) ":" (format nil "~d" (quri:uri-port uri))))
	 (stream (connection-pop uri-dest)))
    (let (response status-code headers response-uri new-stream success)
      (with-retrying (maybe-retry :retries 3
				  :before-retry (sleep 0.2))
	(unwind-protect
	     (handler-bind (((or dex:http-request-failed usocket:ns-condition usocket:socket-condition)
			     (lambda (condition)
			       (if-let ((r (find-restart 'dex:ignore-and-continue condition))
					(ct (gethash "content-type" (dex:response-headers condition))))
				   (if (ppcre:scan "^application/json" ct)
				       (invoke-restart r)
				       (maybe-retry))))))
	       (setf (values response status-code headers response-uri new-stream)
		     (apply 'dex:request uri :use-connection-pool nil :keep-alive t :stream stream args))
	       (unless (eq stream new-stream)
		 (when stream (force-close stream))
		 (setf stream new-stream
		       new-stream nil))
	       (when (<= 500 status-code 599)
		 (maybe-retry)
		 (error 'lw2-connection-error :message (format nil "HTTP status ~A" status-code)))
	       (setf success t))
	  (when (not success)
	    (when (streamp response)
	      (force-close response))
	    (when stream
	      (force-close stream))
	    (setf stream nil))))
      (unwind-protect
	   (funcall fn response)
	(when (streamp response)
	  (close response))
	(when stream ; the connection is reusable
	  (connection-push uri-dest stream))))))

(defun forwarded-header ()
  (let ((addr (and (boundp 'hunchentoot:*request*) (hunchentoot:real-remote-addr))))
    (list-cond (addr "X-Forwarded-For" addr))))

(defun signal-lw2-errors (errors)
  (loop for error in errors
        do (let ((message (cdr (assoc :message error)))
                 (path (cdr (assoc :path error))))
             (unless (and path (> (length path) 1))
               (cond
                 ((search "document_not_found" message) (error 'lw2-not-found-error))
                 ((search "app.missing_document" message) (error 'lw2-not-found-error))
		 ((search "only visible to logged-in users" message) (error 'lw2-login-required-error))
                 ((search "not_allowed" message) (error 'lw2-not-allowed-error))
                 (t (error 'lw2-unknown-error :message message)))))))

(define-backend-function earliest-post-time ()
  (backend-lw2 (load-time-value (local-time:parse-timestring "2005-01-01")))
  (backend-ea-forum (load-time-value (local-time:parse-timestring "2011-11-24"))))

(define-backend-function fixup-lw2-return-value (value)
  (backend-base
   value)
  (backend-lw2-modernized
   (values-list
    (map 'list
         (lambda (x)
           (if (member (car x) '(:result :results :total-count))
               (cdr x)
               x))
         value)))
  (backend-accordius
   value))

(define-backend-function deserialize-query-result (result-source)
  (backend-base
   (let ((string-source (typecase result-source
			  (string
			   result-source)
			  (vector
			   (flexi-streams:make-in-memory-input-stream result-source))
			  (stream
			   (ensure-character-stream result-source)))))
     (json:decode-json-from-source string-source))))

(define-backend-function postprocess-query-result (result)
  (backend-base
   result)
  (backend-lw2-legacy
   (signal-lw2-errors (cdr (assoc :errors result)))
   (fixup-lw2-return-value (cdadr (assoc :data result)))))

(define-backend-function decode-query-result (result-source)
  (backend-base
   (postprocess-query-result
    (deserialize-query-result result-source))))

(defmethod graphql-uri ((backend backend-alignment-forum))
  (if *use-alignment-forum*
      "https://www.alignmentforum.org/graphql"
      (call-next-method)))

(define-backend-function backend-request-headers (auth-token forwarded)
  (backend-websocket-login
   (list-cond* (auth-token :authorization auth-token)
	       (call-next-method)))
  (backend-passport-js-login
   (list-cond* (auth-token :cookie (concatenate 'string "loginToken=" auth-token))
	       (call-next-method)))
  (backend-graphql
   (alist* :content-type "application/json"
	   (if forwarded (forwarded-header)))))

(define-backend-function call-with-backend-response (fn query &key return-type auth-token)
  (backend-graphql
   (call-with-http-response
    fn
    (graphql-uri *current-backend*)
    :method :post
    :headers (backend-request-headers auth-token nil)
    :content (dynamic-let ((q (alist :query query))) (json:encode-json-to-string q))
    :want-stream (not return-type))))

(define-backend-function lw2-graphql-query (query &key auth-token return-type (decoder 'decode-query-result))
  (backend-base
   (do-graphql-debug query)
   (call-with-backend-response
    (ecase return-type
      ((nil) decoder)
      (:string #'identity)
      (:both (lambda (string) (values (funcall decoder string) string))))
    query
    :return-type return-type
    :auth-token auth-token)))

(defun lw2-graphql-query-map (fn data &key auth-token postprocess)
  (multiple-value-bind (map-values queries)
    (loop for d in data
          as out-values = (multiple-value-list (funcall fn d))
          as (out passthrough-p) = out-values
          collect out-values into map-values
          when (not passthrough-p) collect out into queries
          finally (return (values map-values queries)))
    (let* ((query-string
             (with-output-to-string (stream)
               (format stream "{")
               (loop for n from 0
                     for q in queries
                     do (format stream "g~6,'0D:~A " n q))
               (format stream "}")))
           (query-result-data (when queries (lw2-graphql-query query-string :decoder 'deserialize-query-result :auth-token auth-token)))
           (errors (cdr (assoc :errors query-result-data))))
      (signal-lw2-errors errors)
      (values
        (loop as results = (sort (cdr (assoc :data query-result-data)) #'string< :key #'car) then (if passthrough-p results (rest results))
              for (out passthrough-p) in map-values
              as result-data-cell = (first results)
                      as result-data = (if passthrough-p out (fixup-lw2-return-value (cdr result-data-cell)))
                      for input-data in data
                      collect (if postprocess (funcall postprocess input-data result-data) result-data))
                errors))))

(defun lw2-graphql-query-multi (query-list &key auth-token)
  (values-list (lw2-graphql-query-map #'identity query-list :auth-token auth-token)))

(defvar *background-cache-update-threads* (make-hash-table :test 'equal
							   :weakness :value
							   :synchronized t)) 

(defun cache-update (cache-db key data)
  (let ((meta-db (format nil "~A-meta" cache-db))
        (new-hash (hash-string data))
        (current-time (get-unix-time)))
    (with-cache-transaction
      (let* ((metadata (cache-get meta-db key :value-type :lisp))
             (last-mod (if (equalp new-hash (cdr (assoc :city-128-hash metadata)))
                           (or (cdr (assoc :last-modified metadata)) current-time)
                           current-time)))
        (cache-put meta-db key (alist :last-checked current-time :last-modified last-mod :city-128-hash new-hash) :value-type :lisp)
        (cache-put cache-db key data)))))

(defun cache-mark-stale (cache-db key)
  (let ((meta-db (format nil "~A-meta" cache-db))
	(current-time (get-unix-time)))
    (with-cache-transaction
	(let* ((metadata (cache-get meta-db key :value-type :lisp))
	       (metadata (alist* :last-modified current-time (delete :last-modified metadata :key #'car))))
	  (cache-put meta-db key metadata :value-type :lisp)))))

(declaim (type (and fixnum (integer 1)) *cache-stale-factor* *cache-skip-factor*))
(defparameter *cache-stale-factor* 100)
(defparameter *cache-skip-factor* 5000)

(defun cache-is-fresh (cache-db key)
  (let ((metadata (cache-get (format nil "~A-meta" cache-db) key :value-type :lisp))
        (current-time (get-unix-time)))
    (if-let ((last-mod (cdr (assoc :last-modified metadata)))
	     (last-checked (cdr (assoc :last-checked metadata))))
	    (let ((unmodified-time (- last-checked last-mod))
		  (last-checked-time (- current-time last-checked)))
	      (if (> unmodified-time (* *cache-skip-factor* last-checked-time))
		  :skip
		  (> unmodified-time (* *cache-stale-factor* last-checked-time)))))))

(defgeneric run-query (query)
  (:method ((query string))
    (lw2-graphql-query query :return-type :string))
  (:method ((query function))
    (funcall query)))

(declaim (inline make-thread-with-current-backend))

(defun make-thread-with-current-backend (fn &rest args)
  (let ((current-backend *current-backend*))
    (apply #'sb-thread:make-thread
      (lambda ()
        (let ((*current-backend* current-backend))
          (funcall fn)))
      args)))

(defun ensure-cache-update-thread (query cache-db cache-key)
  (let ((key (format nil "~A-~A" cache-db cache-key))) 
    (labels ((background-fn ()
	       (unwind-protect
		    (multiple-value-bind (value error)
			(log-and-ignore-errors
			 (sb-sys:with-deadline (:seconds 60)
			   (nth-value 0
				      (cache-update cache-db cache-key (run-query query)))))
		      (or value error))
		 (remhash key *background-cache-update-threads*))))
      (sb-ext:with-locked-hash-table (*background-cache-update-threads*)
				     (let ((thread (gethash key *background-cache-update-threads*)))
				       (if thread thread
					 (setf (gethash key *background-cache-update-threads*)
					       (make-thread-with-current-backend #'background-fn :name "background cache update"))))))))

(define-backend-function lw2-graphql-query-timeout-cached (query cache-db cache-key &key (decoder 'decode-query-result)
								 (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*))
  (backend-base
   (multiple-value-bind (cached-result is-fresh) (with-cache-readonly-transaction
						     (values (cache-exists cache-db cache-key)
							     (cache-is-fresh cache-db cache-key)))
     (labels ((get-cached-result ()
		(with-cache-readonly-transaction (funcall decoder (cache-get cache-db cache-key :return-type 'binary-stream)))))
       (if (and cached-result (or (not revalidate)
				  (and (not force-revalidate) (eq is-fresh :skip))))
	   (get-cached-result)
	   (let ((timeout (if cached-result
			      (if force-revalidate nil 3)
			      nil))
		 (thread (ensure-cache-update-thread query cache-db cache-key)))
	     (block retrieve-result
	       (if (and cached-result (if force-revalidate (not revalidate) (or is-fresh (not revalidate))))
		   (get-cached-result)
		   (handler-bind
		       ((fatal-error (lambda (c)
				       (declare (ignore c))
				       (if cached-result
					   (return-from retrieve-result (get-cached-result))))))
		     (let ((new-result (sb-thread:join-thread thread :timeout timeout)))
		       (typecase new-result
			 (condition (error new-result))
			 (t (funcall decoder new-result)))))))))))))

(define-backend-function lw2-query-string* (query-type return-type args &key context fields with-total))

(define-backend-operation lw2-query-string* backend-lw2-legacy (query-type return-type args &key context (fields (request-fields query-type return-type context)) with-total)
  (declare (ignore with-total))
  (labels ((lisp-to-lw2-case (x) (json:lisp-to-camel-case (format nil "*~A" x))))
    (graphql-query-string*
     (concatenate 'string
		  (lisp-to-lw2-case query-type)
		  "s"
		  (lisp-to-lw2-case return-type))
     (if (eq return-type :single)
	 args
	 (alist :terms args))
     fields)))

(define-backend-operation lw2-query-string* backend-lw2-modernized (query-type return-type args &key context (fields (request-fields query-type return-type context)) with-total)
  (graphql-query-string*
    (if (eq return-type :single)
	(json:lisp-to-camel-case (string query-type))
	(concatenate 'string (json:lisp-to-camel-case (string query-type)) "s"))
    (alist :input (case return-type
		    (:single (alist :selector args))
		    (:list (alist :enable-total with-total :terms args))
		    (:total (alist :enable-total t :terms args))))
    (case return-type
      (:total '(:total-count))
      (:list (list-cond (t :results fields)
			(with-total :total-count)))
      (:single (alist :result fields)))))

(define-backend-function lw2-query-string (query-type return-type args &key context fields with-total))

(define-backend-operation lw2-query-string backend-lw2-legacy (query-type return-type args &rest rest &key context fields with-total)
  (declare (ignore context fields with-total))
  (format nil "{~A}" (apply 'lw2-query-string* query-type return-type args rest)))

(define-backend-function lw2-query-list-limit-workaround (query-type terms &rest rest &key fields context auth-token)
  (backend-graphql
   (declare (ignore fields context))
   (let (items-list)
     (loop for offset from 0 by 500
	as items-next = (lw2-graphql-query (apply 'lw2-query-string query-type :list (alist* :limit 500 :offset offset terms) (filter-plist rest :fields :context))
					   :auth-token auth-token)
	as length = (length items-next)
	do (setf items-list (nconc items-list items-next))
	while (>= length 500))
     items-list))
  (backend-accordius
   (declare (ignore fields context))
   (lw2-graphql-query (apply 'lw2-query-string query-type :list terms (filter-plist rest :fields :context)) :auth-token auth-token)))

(defun get-cached-index-query (cache-id query)
  (labels ((query-and-put ()
	     (let* ((result (lw2-graphql-query query :return-type :string))
		    (decoded-result (multiple-value-list (decode-query-result result))))
	       (cache-put "index-json" cache-id result)
	       (values-list decoded-result)))
	   (get-cached-result ()
	     (with-cache-readonly-transaction (decode-query-result (cache-get "index-json" cache-id :return-type 'binary-stream)))))
    (let ((cached-result (cache-get "index-json" cache-id :return-type 'existence)))
      (if (and cached-result (background-loader-ready-p))
	  (get-cached-result)
	  (if cached-result
	      (handler-case
		  (query-and-put)
		(t () (get-cached-result)))
	      (query-and-put))))))

(define-backend-function get-posts-index-query-terms (&key view (sort "new") (limit 21) offset before after &allow-other-keys)
  (backend-lw2-legacy
   (let ((sort-key (alexandria:switch (sort :test #'string=)
				      ("new" "new")
				      ("hot" "magic")
				      ("active" "recentComments")
				      ("top" "top")
				      ("old" "old"))))
     (multiple-value-bind (view-terms cache-key)
	 (alexandria:switch (view :test #'string=)
			    ("featured" (alist :sorted-by sort-key :filter "curated"))
			    ("all" (alist :sorted-by sort-key :filter "all"))
			    ("alignment-forum" (alist :sorted-by sort-key :af t))
			    ("questions" (alist :sorted-by sort-key :filter "questions"))
			    ("events" (alist :sorted-by sort-key :filter "events"))
			    ("nominations" (alist :view "nominations2019"))
			    ("reviews" (alist :view "reviews2019"))
			    (t (values
				(alist :sorted-by sort-key :filter "frontpage")
				(if (not (or (string/= sort "new") (/= limit 21) offset before after)) "new-not-meta"))))
       (let ((terms
	      (alist-without-null* :before before
				   :after after
				   :limit limit
				   :offset offset
				   view-terms)))
	 (values terms cache-key))))))

(define-backend-operation get-posts-index-query-terms backend-lw2-tags :around (&key hide-tags &allow-other-keys)
  (multiple-value-bind (query-terms cache-key) (call-next-method)
    (if hide-tags
	(values (acons :filter-settings (alist :tags (list* :list (map 'list (lambda (tagid) (alist :tag-id tagid :filter-mode "Hidden"))
								       hide-tags)))
		       query-terms)
		nil)
	(values query-terms cache-key))))

(define-backend-function get-posts-index-query-string (&rest args &key &allow-other-keys)
  (backend-lw2-legacy
   (declare (dynamic-extent args))
   (multiple-value-bind (query-terms cache-key)
       (apply #'%get-posts-index-query-terms backend args)
     (values (lw2-query-string :post :list query-terms)
	     cache-key))))

(define-backend-function get-posts-index (&rest args &key &allow-other-keys)
  (backend-lw2-legacy
   (declare (dynamic-extent args))
   (multiple-value-bind (query-string cache-key)
       (apply #'%get-posts-index-query-string backend args)
     (if cache-key
	 (get-cached-index-query cache-key query-string)
	 (lw2-graphql-query query-string)))))

(define-backend-operation get-posts-index backend-lw2-tags :around (&rest args &key hide-tags offset (limit 21) &allow-other-keys)
  ;; Workaround for https://github.com/LessWrong2/Lesswrong2/issues/3099
  (declare (dynamic-extent args))
  (if hide-tags
      (let ((offset (or offset 0)))
	(subseq
	 (apply #'call-next-method backend :offset 0 :limit (+ limit offset) args)
	 offset))
      (call-next-method)))

(defun get-posts-json ()
  (lw2-graphql-query (get-posts-index-query-string) :return-type :string))

(defun get-recent-comments (&key with-total)
  (get-cached-index-query "recent-comments" (lw2-query-string :comment :list '((:view . "allRecentComments") (:limit . 20)) :context :index :with-total with-total)))

(defun get-recent-comments-json ()
  (lw2-graphql-query (lw2-query-string :comment :list '((:view . "allRecentComments") (:limit . 20)) :context :index) :return-type :string))

(defun process-vote-result (res)
  (alist-bind ((id (or null simple-string) :--id)
	       current-user-votes
	       current-user-vote
	       current-user-extended-vote)
	      res
	      (let ((karma-vote (or (nonempty-string current-user-vote)
				    (cdr (assoc :vote-type (first current-user-votes))))))
		(values (list-cond* (karma-vote :karma karma-vote)
				    current-user-extended-vote)
			id))))

(defun process-votes-result (res)
  (let ((hash (make-hash-table)))
    (dolist (v res hash)
      (multiple-value-bind (vote id) (process-vote-result v)
	(when vote
	  (setf (gethash id hash) vote))))))

(defun flatten-shortform-comments (comments)
  (let ((output comments))
    (loop for comment in comments do
	 (setf output (append (cdr (assoc :latest-children comment)) output)))
    output))

(defun get-shortform-votes (auth-token &key (offset 0) (limit 20))
  (process-votes-result
   (flatten-shortform-comments
    (lw2-graphql-query (lw2-query-string :comment :list (alist :view "shortform" :offset offset :limit limit)
					 :fields '(:--id :current-user-vote :current-user-extended-vote (:latest-children :--id :current-user-vote :current-user-extended-vote)))
		       :auth-token auth-token))))

(defun get-post-vote (post-id auth-token)
  (process-vote-result (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) :fields '(:--id :current-user-vote :current-user-extended-vote)) :auth-token auth-token)))

(define-cache-database 'backend-lw2-tags "tag-posts" "tag-posts-meta" "post-tags" "post-tags-meta")

(define-backend-function get-tag-posts (slug &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*))
  (backend-base (declare (ignore revalidate force-revalidate)) nil)
  (backend-lw2-tags
   (let* ((tagid (get-slug-tagid slug))
	  (query-fn (lambda ()
		      (comments-list-to-graphql-json
		       (lw2-query-list-limit-workaround
			:tag-rel
			(alist :view "postsWithTag" :tag-id tagid)
			:fields (list (list* :post (request-fields :post :list :index))))))))
     (iter
      (for x in (lw2-graphql-query-timeout-cached query-fn "tag-posts" tagid :revalidate revalidate :force-revalidate force-revalidate))
      (when-let (post (cdr (assoc :post x)))
	(collect post))))))

(define-backend-function get-tag-post-votes (tag-id auth-token)
  (backend-base (progn tag-id auth-token nil))
  (backend-lw2-tags
   (process-votes-result
    (map 'list #'cdr
	 (lw2-query-list-limit-workaround
	  :tag-rel
	  (alist :view "postsWithTag" :tag-id tag-id)
	  :fields '(:--id :current-user-vote :current-user-extended-vote)
	  :auth-token auth-token)))))

(define-backend-function get-post-tags (post-id &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*))
  (backend-base (declare (ignore revalidate force-revalidate)) nil)
  (backend-lw2-tags
   (let ((query-string (lw2-query-string :tag-rel :list (alist :view "tagsOnPost" :post-id post-id) :fields '((:tag :name :slug)))))
     (lw2-graphql-query-timeout-cached query-string "post-tags" post-id :revalidate revalidate :force-revalidate force-revalidate))))

(define-backend-function get-post-tag-votes (post-id auth-token)
  (backend-base (progn post-id auth-token nil))
  (backend-lw2-tags
   (process-votes-result
    (lw2-graphql-query (lw2-query-string :tag-rel :list (alist :view "tagsOnPost" :post-id post-id) :fields '(:--id :current-user-vote :current-user-extended-vote))
		       :auth-token auth-token))))

(define-backend-function get-post-body (post-id &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*) auth-token)
  (backend-graphql
   (let ((query-string (lw2-query-string :post :single (alist :document-id post-id) :context :body)))
     (block nil
       (tagbody retry
	  (handler-bind ((lw2-login-required-error (lambda (&rest args)
						     (declare (ignore args))
						     (let ((current-auth-token *current-auth-token*))
						       (when (and (not auth-token) current-auth-token)
							 (setf auth-token current-auth-token)
							 (go retry))))))
	    (return
	      (if auth-token
		  (lw2-graphql-query query-string :auth-token auth-token)
		  (lw2-graphql-query-timeout-cached query-string "post-body-json" post-id :revalidate revalidate :force-revalidate force-revalidate))))))))
  (backend-lw2-tags
   (declare (ignore auth-token))
   (acons :tags (get-post-tags post-id :revalidate revalidate :force-revalidate force-revalidate) (call-next-method)))
  (backend-magnum-crossposts
   (declare (ignore auth-token))
   (let ((post (call-next-method)))
     (alist-bind (is-crosspost hosted-here foreign-post-id) (cdr (assoc :fm-crosspost post))
       (cond ((not (and (backend-magnum-crosspost-site backend)
			(not (boundp '*retrieve-crosspost*))
			is-crosspost foreign-post-id))
	      post)
	     (:otherwise
	      (let* ((*current-site* (find-site (backend-magnum-crosspost-site backend)))
		     (*current-backend* (site-backend *current-site*))
		     (*retrieve-crosspost* nil)
		     (foreign-post (get-post-body foreign-post-id :revalidate revalidate :force-revalidate force-revalidate))
		     (foreign-post-body (cdr (assoc :html-body foreign-post))))
		(declare (special *retrieve-crosspost*))
		(if foreign-post-body
		    (list-cond*
		     ((not hosted-here) :html-body foreign-post-body)
		     (t :foreign-post foreign-post)
		     post)
		    post))))))))

(defun get-post-comments-list (post-id view &rest rest &key auth-token parent-answer-id fields context)
  (declare (ignore fields context auth-token))
  (let ((terms (alist :view view :post-id post-id)))
    (when parent-answer-id
      (setf terms (acons :parent-answer-id parent-answer-id terms)))
    (apply 'lw2-query-list-limit-workaround :comment terms (filter-plist rest :fields :context :auth-token))))

(defun get-post-answer-replies (post-id answers &rest rest &key auth-token fields context)
  ;; todo: support more than 500 answers per question
  (declare (ignore fields context))
  (let* ((terms (alist :view "repliesToAnswer" :post-id post-id :limit 500))
	 (result (lw2-graphql-query-map
		  #'identity
		  (mapcar (lambda (answer) (apply 'lw2-query-string* :comment :list
						  (acons :parent-answer-id (cdr (assoc :--id answer)) terms)
						  (filter-plist rest :fields :context)))
			  answers)
		  :auth-token auth-token)))
    (apply #'nconc result)))

(define-backend-function get-post-comments-votes (post-id auth-token)
  (backend-graphql
   (let ((fields '(:--id :current-user-vote :current-user-extended-vote)))
     (process-votes-result
      (get-post-comments-list post-id "postCommentsTop" :auth-token auth-token :fields fields))))
  (backend-q-and-a
   (let* ((fields '(:--id :current-user-vote :current-user-extended-vote))
	  (answers (get-post-comments-list post-id "questionAnswers" :auth-token auth-token :fields fields)))
     (process-votes-result
      (nconc
       (get-post-comments-list post-id "postCommentsTop" :auth-token auth-token :fields fields)
       (get-post-answer-replies post-id answers :auth-token auth-token :fields fields)
       answers)))))

(define-backend-function get-tag-comments-votes (tag-id auth-token)
  (backend-lw2-tags-comments
   (process-votes-result (lw2-graphql-query (lw2-query-string :comment :list (alist :tag-id tag-id :view "tagDiscussionComments") :fields '(:--id :current-user-vote :current-user-extended-vote))
					    :auth-token auth-token))))

(define-backend-function get-post-comments (post-id &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*))
  (backend-graphql
   (let ((fn (lambda ()
	       (comments-list-to-graphql-json
		(get-post-comments-list post-id "postCommentsTop")))))
     (lw2-graphql-query-timeout-cached fn "post-comments-json" post-id :revalidate revalidate :force-revalidate force-revalidate)))
  (backend-ea-forum
   ;; Work around bizarre parent comment bug in EA forum
   (declare (ignore revalidate force-revalidate))
   (let ((comments (call-next-method)))
     (dolist (c comments)
       (if-let (parent-id-cons (assoc :parent-comment-id c))
	       (if (and (string= (cdr parent-id-cons) "rjgZaK8uzHG3jAu2p")
			(not (string= post-id "h26Kx7uGfQfNewi7d")))
		   (setf (cdr parent-id-cons) nil))))
     comments)))

(defun get-post-answers (post-id &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*))
  (let ((fn (lambda ()
	      (let ((answers (get-post-comments-list post-id "questionAnswers")))
		(comments-list-to-graphql-json
		 (nconc
		  answers
		  (get-post-answer-replies post-id answers)))))))
    (lw2-graphql-query-timeout-cached fn "post-answers-json" post-id :revalidate revalidate :force-revalidate force-revalidate)))

(defun sequence-iterate (sequence fn)
  (dolist (chapter (cdr (assoc :chapters sequence)))
    (dolist (post (cdr (assoc :posts chapter)))
      (funcall fn post))))

(defun sequence-post-ids (sequence)
  (with-collector (col)
    (sequence-iterate sequence
		      (lambda (post)
			(col (cdr (assoc :--id post)))))
    (col)))

(defun get-sequence-post (sequence post-id)
  (sequence-iterate sequence
		    (lambda (post)
		      (when (string= (cdr (assoc :--id post)) post-id)
			(return-from get-sequence-post post))))
  nil)

(define-backend-function get-sequence (sequence-id)
  (backend-graphql
   (let ((fn (lambda ()
	       (multiple-value-bind (sequence sequence-json)
		   (lw2-graphql-query
		    (lw2-query-string :sequence :single
				      (alist :document-id sequence-id)
				      :fields `(:--id :title :created-at :user-id
						      (:contents :html)
						      (:chapters :title :subtitle :number (:contents :html) (:posts ,@(request-fields :post :list nil)))
						      :grid-image-id :----typename))
		    :return-type :both)
		 (let ((posts (sequence-post-ids sequence)))
		   (with-cache-transaction
		       (dolist (post-id posts)
			 (let ((old-seqs (cache-get "post-sequence" post-id :value-type :json)))
			   (unless (member sequence-id old-seqs :test #'string=)
			     (cache-put "post-sequence" post-id (cons sequence-id old-seqs) :value-type :json)))))
		   sequence-json)))))
     (lw2-graphql-query-timeout-cached fn "sequence-json" sequence-id))))

(define-backend-function get-post-sequence-ids (post-id)
  (backend-graphql
   (cache-get "post-sequence" post-id :value-type :json)))

(defun preload-sequences-cache ()
  (declare (optimize space (compilation-speed 2) (speed 0)))
  (let ((sequences (apply #'append
			  (loop for view in '("curatedSequences" "communitySequences")
			     collect (lw2-graphql-query (lw2-query-string :sequence :list (alist :view view) :fields '(:--id)))))))
    (dolist (sequence sequences)
      (get-sequence (cdr (assoc :--id sequence))))
    (format t "Retrieved ~A sequences." (length sequences)))
  (values))

(define-backend-function user-deleted (user-id &optional (status nil set))
  (backend-base
   (declare (ignore user-id status set))
   nil)
  (backend-lw2-modernized
   (if set
       (if status
	   (cache-put "user-deleted" user-id "1")
	   (cache-del "user-deleted" user-id))
       (cache-get "user-deleted" user-id :return-type 'existence))))

(define-backend-function get-user (user-identifier-type user-identifier &key (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*) auth-token)
  (backend-graphql
   (let* ((user-id (ccase user-identifier-type
		     (:user-id user-identifier)
		     (:user-slug (get-slug-userid user-identifier))))
	  (query-string (lw2-query-string :user :single (alist :document-id user-id) :fields (list-cond* (auth-token :last-notifications-check) (user-fields))))
	  (result (if auth-token
		      (lw2-graphql-query query-string :auth-token auth-token)
		      (lw2-graphql-query-timeout-cached query-string "user-json" user-id :revalidate revalidate :force-revalidate force-revalidate))))
     (alist-bind ((user-id (or simple-string null) :--id)
		  (display-name (or simple-string null))
		  (full-name (or simple-string null))
		  (slug (or simple-string null))
		  (deleted boolean))
		 result
		 (when user-id
	 (with-cache-transaction
	   (when display-name
	     (cache-username user-id display-name))
	   (when full-name
	     (cache-user-full-name user-id full-name))
	   (when slug
	     (cache-user-slug user-id slug)
	     (cache-slug-userid slug user-id))
	   (user-deleted user-id deleted))))
     result)))

(define-backend-function get-notifications (&key user-id (offset 0) (limit 21) auth-token)
  (backend-lw2-legacy
   (lw2-graphql-query (lw2-query-string :notification :list
					(alist* :user-id user-id :limit limit :offset offset *notifications-base-terms*)
					:fields '(:--id :document-type :document-id :link :title :message :type :viewed))
		      :auth-token auth-token))
  (backend-lw2-modernized
   (declare (ignore user-id offset limit auth-token))
   (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
     (call-next-method))))

(define-backend-function check-notifications (user-id auth-token &key full since)
  (backend-lw2-legacy
   (multiple-value-bind (notifications user-info)
       (lw2-graphql-query-multi (list
				 (lw2-query-string* :notification :list (alist* :user-id user-id :limit (if full 3 1) *notifications-base-terms*)
						    :fields (if full '(:--id :message :created-at) '(:created-at)))
				 (lw2-query-string* :user :single (alist :document-id user-id) :fields '(:last-notifications-check)))
				:auth-token auth-token)
     (let ((last-check (or since
			   (let ((last-check-string (cdr (assoc :last-notifications-check user-info))))
			     (when (and (stringp last-check-string) (not (equal last-check-string "")))
			       (local-time:parse-timestring last-check-string))))))
       (when notifications
	 (labels ((unread-p (notification)
		    (if last-check
			(local-time:timestamp>
			 (local-time:parse-timestring (cdr (assoc :created-at notification)))
			 last-check)
			;; User has never checked notifications before -- all are unread
			t)))
	   (if full
	       (remove-if-not #'unread-p notifications)
	       (unread-p (first notifications))))))))
  (backend-lw2-modernized
   (declare (ignore user-id auth-token full since))
   (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
     (call-next-method))))

(define-cache-database 'backend-lw2-legacy "comment-reply-by-user")

(define-backend-function mark-comment-replied (reply)
  (backend-lw2-legacy
   (alexandria:when-let* ((parent-comment-id (cdr (assoc :parent-comment-id reply)))
			  (reply-id (cdr (assoc :--id reply)))
			  (user-id (cdr (assoc :user-id reply))))
     (cache-put "comment-reply-by-user" (concatenate 'string parent-comment-id " " user-id) reply-id))))

(define-backend-function check-comment-replied (comment-id user-id)
  (backend-lw2-legacy
   (cache-get "comment-reply-by-user" (concatenate 'string comment-id " " user-id))))

(define-backend-function get-user-page-items (user-id request-type &key (offset 0) (limit 21) (sort-type :date) drafts
						      (revalidate *revalidate-default*) (force-revalidate *force-revalidate-default*) auth-token)
  (backend-lw2-legacy
   (multiple-value-bind (real-offset real-limit) (if (eq request-type :both)
						     (values 0 (+ offset limit))
						     (values offset limit))
     (let* ((cache-database (when (and (eq request-type :both)
				       (or (not offset) (= offset 0))
				       (= limit 21)
				       (eq sort-type :date)
				       (not drafts)
				       (not auth-token))
			      "user-page-items"))
	    (return-type (if cache-database :string nil))
	    (fn (lambda ()
		  (labels ((posts-query-string ()
			     (let* ((base-terms
				     (cond
				       (drafts (alist :view "drafts"))
				       ((eq sort-type :score) (alist :view "top"))
				       ((eq sort-type :date-reverse) (alist :view "old"))
				       (t (alist :view "userPosts"))))
				    (terms (alist* :offset real-offset :limit real-limit :user-id user-id base-terms)))
			       (declare (dynamic-extent base-terms terms))
			       (lw2-query-string* :post :list terms)))
			   (comments-query-string ()
			     (let* ((view (ecase sort-type
						    (:score "postCommentsTop")
						    (:date "allRecentComments")
						    (:date-reverse "postCommentsOld")))
				    (terms (alist :offset real-offset
						  :limit real-limit
						  :user-id user-id
						  :view view)))
			       (declare (dynamic-extent view terms))
			       (lw2-query-string* :comment :list terms
						  :context :index))))
		    (declare (dynamic-extent #'posts-query-string #'comments-query-string))
		    (case request-type
		      (:both (let ((result (multiple-value-call #'concatenate 'list
								(lw2-graphql-query-multi (list (posts-query-string) (comments-query-string))))))
			       (ecase return-type
				 (:string (json:encode-json-to-string result))
				 ((nil) result))))
		      (:posts (lw2-graphql-query (format nil "{~A}" (posts-query-string)) :auth-token auth-token :return-type return-type))
		      (:comments (lw2-graphql-query (format nil "{~A}" (comments-query-string)) :auth-token auth-token :return-type return-type)))))))
       (if cache-database
	   (lw2-graphql-query-timeout-cached fn cache-database user-id :decoder 'deserialize-query-result :revalidate revalidate :force-revalidate force-revalidate)
	   (funcall fn))))))

(define-backend-function get-conversation-messages (conversation-id auth-token)
  (backend-lw2-legacy
   (lw2-graphql-query-multi
    (list
     (lw2-query-string* :conversation :single (alist :document-id conversation-id) :fields '(:title (:participants :display-name :slug)))
     (lw2-query-string* :message :list (alist :view "messagesConversation" :conversation-id conversation-id) :fields *messages-index-fields*))
    :auth-token auth-token)))

(define-backend-function lw2-search-query (query)
  (backend-algolia-search
   (call-with-http-response
    (lambda (req-stream)
      (values-list (loop for r in (cdr (assoc :results (json:decode-json req-stream)))
		      collect (cdr (assoc :hits r)))))
    (algolia-search-uri *current-backend*)
    :method :post
    :headers '(("Origin" . "https://www.greaterwrong.com")
	       ("Referer" . "https://www.greaterwrong.com/")
	       ("Content-Type" . "application/json"))
    :content (json:encode-json-alist-to-string
	      (alist "requests" (loop for index in '("test_tags" "test_posts" "test_comments")
				   collect (alist "indexName" index
						  "params" (format nil "query=~A&hitsPerPage=20&page=0"
								   (url-rewrite:url-encode query))))))
    :want-stream t)))

(define-backend-function get-username-wrapper (user-id fn)
  (backend-base
   (funcall fn user-id))
  (backend-lw2-modernized
   (if (user-deleted user-id)
       "[deleted]"
       (funcall fn user-id))))

(define-cache-database 'backend-lw2-legacy "comment-markdown-source" "post-markdown-source")

(defun markdown-source-db-name (target-type)
  (ecase target-type (:comment "comment-markdown-source") (:post "post-markdown-source")))

(define-backend-function markdown-source (target-type id version)
  (backend-lw2-modernized
   (let ((db-name (markdown-source-db-name target-type))
	 (version (base64:usb8-array-to-base64-string (hash-string version))))
     (or
      (if-let ((cache-data (cache-get db-name id :value-type :lisp)))
	      (alist-bind ((cached-version simple-string :version)
			   (markdown simple-string))
			  cache-data
			  (when (string= version cached-version)
			    markdown)))
      (trivia:ematch (lw2-graphql-query (lw2-query-string target-type :single
							  (alist :document-id id)
							  :fields '(:html-body (:contents :markdown)))
					:auth-token *current-auth-token*)
		     ((trivia:alist (:html-body . html-body)
				    (:contents . (assoc :markdown markdown)))
		      (cache-put db-name id (alist :version (base64:usb8-array-to-base64-string (hash-string html-body)) :markdown markdown) :value-type :lisp)
		      markdown))))))

(define-backend-function (setf markdown-source) (markdown target-type id version)
  (backend-lw2-modernized
   (let ((version (base64:usb8-array-to-base64-string (hash-string version))))
     (cache-put (markdown-source-db-name target-type)
		id
		(alist :version version :markdown markdown)
		:value-type :lisp))))

(defun get-elicit-question-title (question-id)
  (cdr
   (lw2-graphql-query (graphql-query-string "ElicitBlockData" (alist :question-id question-id) '(:title)))))

(defun make-rate-limiter (delay)
  (let ((rl-hash (make-hash-table :test 'equal :synchronized t)))
    (lambda (datum fn)
      (let ((unix-time (get-unix-time)))
        (if (sb-ext:with-locked-hash-table (rl-hash)
              (maphash (lambda (k v)
                         (if (> (- unix-time v) delay)
                             (remhash k rl-hash)))
                       rl-hash)
              (not (gethash datum rl-hash)))
            (progn
              (setf (gethash datum rl-hash) unix-time)
              (funcall fn))
            (error "Request aborted due to rate limit."))))))

(defmacro with-rate-limit (&body outer-body)
    `(let ((rate-limiter (make-rate-limiter 30)))
       (macrolet ((rate-limit ((key) &body inner-body)
                    `(funcall rate-limiter ,key (lambda () ,@inner-body))))
         ,@outer-body)))

(with-rate-limit
  (simple-cacheable ("post-title" 'backend-lw2-legacy "postid-to-title" post-id)
    (rate-limit (post-id) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) :fields '(:title)))))))) 

(with-rate-limit
  (simple-cacheable ("post-slug" 'backend-lw2-legacy "postid-to-slug" post-id)
    (rate-limit (post-id) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) :fields '(:slug))))))))

(with-rate-limit
  (simple-cacheable ("slug-postid" 'backend-lw2-legacy "slug-to-postid" slug)
    (rate-limit (slug) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :slug slug) :fields '(:--id))))))))

(with-rate-limit
  (simple-cacheable ("username" 'backend-lw2-legacy "userid-to-displayname" user-id :get-wrapper #'get-username-wrapper)
    (rate-limit (user-id) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) :fields '(:display-name)))))))) 

(with-rate-limit
  (simple-cacheable ("user-slug" 'backend-lw2-legacy "userid-to-slug" user-id)
    (rate-limit (user-id) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) :fields '(:slug))))))))

(with-rate-limit
  (simple-cacheable ("user-full-name" 'backend-lw2-legacy "userid-to-full-name" user-id)
    (rate-limit (user-id) (or (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) :fields '(:full-name)))))
			      ""))))

(with-rate-limit
  (simple-cacheable ("slug-userid" 'backend-lw2-legacy "slug-to-userid" slug)
    (rate-limit (slug) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :slug slug) :fields '(:--id))))))))

(with-rate-limit
  (simple-cacheable ("slug-tagid" 'backend-lw2-tags "slug-to-tagid" slug :catch-errors nil)
    (rate-limit (slug) (cdr (first (first (lw2-graphql-query (lw2-query-string :tag :list (alist :view "tagBySlug" :slug slug) :fields '(:--id)))))))))

(defun preload-username-cache ()
  (declare (optimize space (compilation-speed 2) (speed 0)))
  (let ((user-list (lw2-graphql-query (lw2-query-string :user :list '() :fields '(:--id :slug :display-name)))))
    (with-cache-transaction
	(loop for user in user-list
	   do (alist-bind ((user-id (or simple-string null) :--id)
			   (slug (or simple-string null))
			   (display-name (or simple-string null)))
			  user
		(when user-id
		  (when display-name
		    (cache-username user-id display-name))
		  (when slug
		    (cache-user-slug user-id slug)
		    (cache-slug-userid slug user-id))))))))
