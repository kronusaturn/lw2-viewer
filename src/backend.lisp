(uiop:define-package #:lw2.backend
  (:use #:cl #:sb-thread #:flexi-streams #:alexandria #:lw2-viewer.config #:lw2.sites #:lw2.context #:lw2.graphql #:lw2.lmdb #:lw2.utils #:lw2.hash-utils #:lw2.backend-modules #:lw2.schema-type)
  (:import-from #:collectors #:with-collector)
  (:reexport #:lw2.backend-modules)
  (:export #:*graphql-debug-output*
           #:posts-index-fields #:*messages-index-fields*
	   #:comments-index-fields 
           #:*notifications-base-terms*
           #:condition-http-return-code
           #:lw2-error #:lw2-client-error #:lw2-not-found-error #:lw2-user-not-found-error #:lw2-not-allowed-error #:lw2-server-error #:lw2-connection-error #:lw2-unknown-error
	   #:log-condition #:log-conditions #:start-background-loader #:stop-background-loader #:background-loader-running-p
	   #:lw2-graphql-query-streamparse #:lw2-graphql-query-noparse #:decode-graphql-json #:lw2-graphql-query
           #:lw2-query-string* #:lw2-query-string
           #:lw2-graphql-query-map #:lw2-graphql-query-multi
	   #:get-posts-index #:get-posts-json #:get-post-body #:get-post-vote #:get-post-comments #:get-post-answers #:get-post-comments-votes #:get-recent-comments #:get-recent-comments-json
	   #:sequence-post-ids #:get-sequence #:get-post-sequences #:get-sequence-post
	   #:get-conversation-messages
	   #:get-user
           #:get-notifications #:check-notifications
	   #:lw2-search-query #:get-post-title #:get-post-slug #:get-slug-postid #:get-username #:get-user-full-name #:get-user-slug
	   #:do-wl-rest-mutate #:do-wl-rest-query #:do-wl-create-tag)
  (:recycle #:lw2-viewer)
  (:unintern #:get-posts #:make-posts-list-query #:define-backend-fields
	     #:*posts-index-fields*))

(in-package #:lw2.backend)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defvar *graphql-debug-output* nil)

(defparameter *comments-index-fields* '(:--id :user-id :post-id :posted-at :parent-comment-id (:parent-comment :--id :user-id :post-id) :base-score :page-url :vote-count :retracted :deleted-public :html-body))
(defparameter *post-comments-fields* '(:--id :user-id :post-id :posted-at :parent-comment-id :base-score :page-url :vote-count :retracted :deleted-public :html-body))
(defparameter *messages-index-fields* '(:--id :user-id :created-at (:contents :html) (:conversation :--id :title) :----typename))
(defparameter *user-fields* '(:--id :slug :display-name :karma :groups))

(defparameter *notifications-base-terms* (alist :view "userNotifications" :created-at :null :viewed :null))

(defmacro define-index-fields (function-name schema-type-name &key qualifier)
  (let (main-fields
	backend-specific-fields
	(schema-type (find-schema-type schema-type-name)))
    (dolist (field (cdr (assoc :fields schema-type)))
      (destructuring-bind (field-name field-type &key alias backend-type graphql-ignore subfields ((:qualifier field-qualifier)) &allow-other-keys) field
	(declare (ignore field-type))
	(let* ((field-name (intern (string (or alias field-name)) '#:keyword))
	       (field-designator (if subfields
				     (list* field-name subfields)
				     field-name)))
	  (when (and (not graphql-ignore) (or (not field-qualifier) (string= qualifier field-qualifier)))
	    (if backend-type
		(let ((cons (or (assoc backend-type backend-specific-fields)
				(first (push (cons backend-type nil) backend-specific-fields)))))
		  (push field-designator (cdr cons)))
		(push field-designator main-fields))))))
    `(define-backend-function ,function-name ()
       (backend-graphql ',main-fields)
       ,@(mapcar (lambda (fitem)
		   (destructuring-bind (backend-name . fields) fitem
		   `(,(intern (string backend-name) *package*) (list* ,@fields (call-next-method)))))
		 backend-specific-fields))))

(define-index-fields posts-index-fields :post)
(define-index-fields comments-index-fields :comment :qualifier :index)
(define-index-fields post-comments-fields :comment)

(define-backend-function user-fields ()
  (backend-graphql (load-time-value *user-fields*))
  (backend-alignment-forum (load-time-value (append *user-fields* '(:af-karma :full-name)))))

(define-cache-database
    "index-json"
    "post-comments-json" "post-comments-json-meta" "post-answers-json" "post-answers-json-meta"
    "post-body-json" "post-body-json-meta"
    "sequence-json" "sequence-json-meta" "post-sequence"
    "user-json" "user-json-meta")

(defmethod condition-http-return-code ((c condition)) 500)

(define-condition lw2-error (error) ((http-return-code :allocation :class :reader condition-http-return-code :initform 503)))

(define-condition lw2-client-error (lw2-error) ((http-return-code :allocation :class :initform 400)))

(define-condition lw2-not-found-error (lw2-client-error) ((http-return-code :allocation :class :initform 404))
  (:report "LW server reports: document not found."))

(define-condition lw2-user-not-found-error (lw2-not-found-error) ()
  (:report "User not found."))

(define-condition lw2-not-allowed-error (lw2-client-error) ((http-return-code :allocation :class :initform 403))
  (:report "LW server reports: not allowed."))

(define-condition lw2-server-error (lw2-error) ())

(define-condition lw2-connection-error (lw2-server-error)
  ((message :initarg :message :reader lw2-server-error-message))
  (:report (lambda (c s)
             (format s "Unable to connect to LW server: ~A" (lw2-server-error-message c)))))

(define-condition lw2-unknown-error (lw2-server-error)
  ((message :initarg :message :reader lw2-unknown-error-message))
  (:report (lambda (c s)
             (format s "Unrecognized LW server error: ~A" (lw2-unknown-error-message c)))))

(defun log-condition (condition)
  (with-open-file (outstream "./logs/error.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format outstream "~%~A: ~S ~A~%" (local-time:format-timestring nil (local-time:now)) condition condition)
    (sb-debug:print-backtrace :stream outstream :from :interrupted-frame :print-frame-source t))) 

(defmacro log-conditions (&body body)
  `(block log-conditions
     (handler-bind
       (((or warning serious-condition) (lambda (c) (log-condition c))))
       (progn ,@body))))

(define-backend-function comments-list-to-graphql-json (comments-list)
  (backend-lw2-legacy
   (json:encode-json-to-string
    (json:with-local-class-registry ()
      (json:make-object `((data . ,(json:make-object `((*comments-list . ,comments-list)) nil))) nil))))
  (backend-lw2-modernized
   (json:encode-json-to-string
    (json:with-local-class-registry ()
      (json:make-object `((data . ,(json:make-object `((*comments-list . ,(json:make-object `((results . ,comments-list)) nil))) nil))) nil)))))

(defvar *background-loader-thread* nil)
(defvar *background-loader-semaphore* (make-semaphore :count 1))
(defvar *background-loader-ready* nil)

(defun background-loader-running-p ()
  (case (semaphore-count *background-loader-semaphore*)
    (0 t)
    (1 nil)))

(defun background-loader-ready-p ()
  (and (background-loader-running-p)
       (background-loader-enabled *current-site*)
       *background-loader-ready*))

(defun make-site-background-loader-fn (site)
  (let (last-comment-processed)
    (lambda ()
      (with-site-context (site)
	(handler-case
	    (log-conditions
	     (let ((posts-json (sb-sys:with-deadline (:seconds 120) (get-posts-json))))
	       (when (and posts-json (ignore-errors (json:decode-json-from-string posts-json)))
		 (cache-put "index-json" "new-not-meta" posts-json)
		 (let ((posts-list (decode-graphql-json posts-json)))
		   (with-db (db "postid-to-title")
		     (dolist (post posts-list)
		       (lmdb-put-string db (cdr (assoc :--id post)) (cdr (assoc :title post)))))
		   (with-db (db "postid-to-slug")
		     (dolist (post posts-list)
		       (lmdb-put-string db (cdr (assoc :--id post)) (cdr (assoc :slug post)))))))))
	  (t (condition) (values nil condition)))
	(handler-case
	    (log-conditions
	     (let ((recent-comments-json (sb-sys:with-deadline (:seconds 120) (get-recent-comments-json))))
	       (when-let (recent-comments (ignore-errors (decode-graphql-json recent-comments-json)))
			 (cache-put "index-json" "recent-comments" recent-comments-json)
			 (loop for comment in recent-comments
			    as comment-id = (cdr (assoc :--id comment))
			    as cache-database = (if (or (cdr (assoc :answer comment)) (cdr (assoc :parent-answer-id comment)))
						    "post-answers-json"
						    "post-comments-json")
			    if (string= comment-id last-comment-processed) return nil
			    do
			      (with-cache-transaction
				  (let* ((post-id (cdr (assoc :post-id comment)))
					 (post-comments (ignore-errors (decode-graphql-json (cache-get cache-database post-id))))
					 (new-post-comments (sort (cons comment (delete-if (lambda (c) (string= comment-id (cdr (assoc :--id c)))) post-comments))
								  #'> :key (lambda (c) (cdr (assoc :base-score c))))))
				    (cache-update cache-database post-id (comments-list-to-graphql-json new-post-comments)))))
			 (setf last-comment-processed (cdr (assoc :--id (first recent-comments)))))))
	  (t (condition) (values nil condition)))))))

(defun background-loader ()
  (let (sites loader-functions)
    (loop
       (unless (eq sites *sites*)
	 (setf sites *sites*
	       loader-functions (loop for site in sites
				   when (background-loader-enabled site)
				   collect (make-site-background-loader-fn site))))
       (dolist (loader-fn loader-functions)
	 (funcall loader-fn))
       (setf *background-loader-ready* t)
       (if (wait-on-semaphore *background-loader-semaphore* :timeout 60)
	   (return)))))

(defun start-background-loader ()
  (if (background-loader-running-p)
      (warn "Background loader already running.")
      (progn
        (wait-on-semaphore *background-loader-semaphore*)
        (setf *background-loader-thread* (sb-thread:make-thread #'background-loader)))))

(defun stop-background-loader ()
  (if (background-loader-running-p)
      (progn
        (signal-semaphore *background-loader-semaphore*)
        (join-thread *background-loader-thread*)
        (setf *background-loader-thread* nil
	      *background-loader-ready* nil)
        (signal-semaphore *background-loader-semaphore*))
      (warn "Background loader not running.")))

(defun do-graphql-debug (query)
  (when *graphql-debug-output*
    (format *graphql-debug-output* "~&GraphQL query: ~A~%" query)))

(defun lw2-graphql-query-streamparse (query &key auth-token)
  (do-graphql-debug query)
  (multiple-value-bind (req-stream status-code headers final-uri reuse-stream want-close)
    (drakma:http-request (graphql-uri *current-backend*) :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil)
			 :want-stream t :close t)
    (declare (ignore status-code headers final-uri reuse-stream))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (unwind-protect
      (json:decode-json req-stream)
      (if want-close (close req-stream)))))

(defun lw2-graphql-query-noparse (query &key auth-token)
  (do-graphql-debug query)
  (multiple-value-bind (response-body status-code headers final-uri reuse-stream want-close status-string)
    (drakma:http-request (graphql-uri *current-backend*) :parameters (list (cons "query" query))
                         :cookie-jar *cookie-jar* :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil)
                         :want-stream nil :close t)
    (declare (ignore headers final-uri reuse-stream want-close))
    (cond
      ((= status-code 200)
       (octets-to-string response-body :external-format :utf-8))
      ((= status-code 400)
       (decode-graphql-json (octets-to-string response-body :external-format :utf-8)))
      (t
	(error "Error while contacting LW2: ~A ~A" status-code status-string)))))

(defun signal-lw2-errors (errors)
  (loop for error in errors
        do (let ((message (cdr (assoc :message error)))
                 (path (cdr (assoc :path error))))
             (unless (and path (> (length path) 1))
               (cond
                 ((search "document_not_found" message) (error (make-condition 'lw2-not-found-error)))
                 ((search "not_allowed" message) (error (make-condition 'lw2-not-allowed-error)))
                 (t (error (make-condition 'lw2-unknown-error :message message))))))))

(define-backend-function fixup-lw2-return-value (value)
  (backend-lw2-legacy
   value)
  (backend-lw2-modernized
   (values-list
    (map 'list
         (lambda (x)
           (if (member (car x) '(:result :results :total-count))
               (cdr x)
               x))
         value))))

(defun decode-graphql-json (json-string)
  (let* ((decoded (json:decode-json-from-string json-string))
	 (errors (cdr (assoc :errors decoded))))
    (signal-lw2-errors errors)
    (fixup-lw2-return-value (cdadr (assoc :data decoded)))))

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
           (query-result-data (when queries (lw2-graphql-query-streamparse query-string :auth-token auth-token)))
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

(defun lw2-graphql-query (query &key auth-token)
  (decode-graphql-json (lw2-graphql-query-noparse query :auth-token auth-token))) 

(defvar *background-cache-update-threads* (make-hash-table :test 'equal
							   :weakness :value
							   :synchronized t)) 

(defun cache-update (cache-db key data)
  (let ((meta-db (format nil "~A-meta" cache-db))
        (new-hash (hash-string data))
        (current-time (get-unix-time)))
    (with-cache-transaction
      (let* ((metadata (if-let (m-str (cache-get meta-db key)) (read-from-string m-str)))
             (last-mod (if (equalp new-hash (cdr (assoc :city-128-hash metadata)))
                           (or (cdr (assoc :last-modified metadata)) current-time)
                           current-time)))
        (cache-put meta-db key (prin1-to-string `((:last-checked . ,current-time) (:last-modified . ,last-mod) (:city-128-hash . ,new-hash))))
        (cache-put cache-db key data)))))

(declaim (type (and fixnum (integer 1)) *cache-stale-factor*))
(defparameter *cache-stale-factor* 20)

(defun cache-is-fresh (cache-db key)
  (let ((metadata (if-let (m-str (cache-get (format nil "~A-meta" cache-db) key)) (read-from-string m-str)))
        (current-time (get-unix-time)))
    (if-let ((last-mod (cdr (assoc :last-modified metadata)))
             (last-checked (cdr (assoc :last-checked metadata))))
            (> (- last-checked last-mod) (* *cache-stale-factor* (- current-time last-checked))))))

(defun run-query (query)
  (etypecase query
    (string (lw2-graphql-query-noparse query))
    (function (funcall query))))

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
			    (handler-case 
			      (prog1
				(cache-update cache-db cache-key (run-query query))
				(remhash key *background-cache-update-threads*))
			      (serious-condition (c)
				 (remhash key *background-cache-update-threads*)
				 (log-condition c)
				 (sb-thread:return-from-thread c))))) 
      (sb-ext:with-locked-hash-table (*background-cache-update-threads*)
				     (let ((thread (gethash key *background-cache-update-threads*)))
				       (if thread thread
					 (setf (gethash key *background-cache-update-threads*)
					       (make-thread-with-current-backend #'background-fn))))))))

(defun lw2-graphql-query-timeout-cached (query cache-db cache-key &key (revalidate t) force-revalidate)
    (multiple-value-bind (cached-result is-fresh) (with-cache-readonly-transaction (values (cache-get cache-db cache-key) (cache-is-fresh cache-db cache-key)))
      (if (and cached-result (not revalidate))
          (decode-graphql-json cached-result)
          (let ((timeout (if cached-result
			     (if force-revalidate nil 3)
			     nil))
                (thread (ensure-cache-update-thread query cache-db cache-key)))
            (decode-graphql-json
	     (if (and cached-result (if force-revalidate (not revalidate) (or is-fresh (not revalidate))))
		 cached-result
		 (handler-case
		     (let ((new-result (sb-thread:join-thread thread :timeout timeout)))
		       (typecase new-result
			 (condition (signal new-result))
			 (t new-result)))
		   (condition (c)
		     (or cached-result
			 (error "Failed to load ~A ~A and no cached version available: ~A" cache-db cache-key c))))))))))

(define-backend-function lw2-query-string* (query-type return-type args fields &key with-total))

(define-backend-operation lw2-query-string* backend-lw2-legacy (query-type return-type args fields &key with-total)
  (declare (ignore with-total))
  (graphql-query-string*
    (concatenate 'string (string-capitalize query-type)
                         "s"
                         (string-capitalize return-type))
    (if (eq return-type :single)
        args
        (alist :terms args))
    fields))

(define-backend-operation lw2-query-string* backend-lw2-modernized (query-type return-type args fields &key with-total)
  (graphql-query-string*
    (if (eq return-type :single)
        (string-downcase query-type)
        (concatenate 'string (string-downcase query-type) "s"))
    (alist :input (case return-type
                      (:single (alist :selector args))
                      (:list (alist :enable-total with-total :terms args))
                      (:total (alist :enable-total t :terms args))))
    (case return-type
        (:total '(:total-count))
        (:list (nconc (list (cons :results fields)) (if with-total '(:total-count))))
        (:single (list (cons :result fields))))))

(define-backend-function lw2-query-string (query-type return-type args fields &key with-total))

(define-backend-operation lw2-query-string backend-lw2-legacy (query-type return-type args fields &key with-total)
  (format nil "{~A}" (lw2-query-string* query-type return-type args fields :with-total with-total)))

(defun get-cached-index-query (cache-id query)
  (labels ((query-and-put ()
             (let* ((result (lw2-graphql-query-noparse query))
                    (decoded-result (multiple-value-list (decode-graphql-json result))))
               (cache-put "index-json" cache-id result)
               (values-list decoded-result))))
    (let ((cached-result (cache-get "index-json" cache-id)))
      (if (and cached-result (background-loader-ready-p))
        (decode-graphql-json cached-result)
        (if cached-result
            (handler-case
              (query-and-put)
              (t () (decode-graphql-json cached-result)))
            (query-and-put))))))

(define-backend-function get-posts-index-query-string (&key view (sort "new") (limit 20) offset before after)
  (backend-lw2-legacy
   (multiple-value-bind (view-terms cache-key)
    (alexandria:switch (view :test #'string=)
                       ("featured" (alist :view "curated"))
                       ("all" (alist :view (if (string= sort "hot") "community" "new") :meta :null))
                       ("meta" (alist :view "new" :meta t :all t))
                       ("community" (alist :view "new" :meta t :all t))
                       ("alignment-forum" (alist :view "new" :af t))
		       ("questions" (alist :view "questions"))
                       (t (values (alist :view (if (string= sort "hot") "frontpage" "frontpage-rss")) (if (not (or (string/= sort "new") (/= limit 20) offset before after)) "new-not-meta"))))
    (let* ((extra-terms
             (remove-if (lambda (x) (null (cdr x)))
                        (alist :before before :after after :limit limit :offset offset)))
           (query-string (lw2-query-string :post :list (nconc view-terms extra-terms) (posts-index-fields))))
      (values query-string cache-key)))))

(define-backend-function get-posts-index (&rest args &key &allow-other-keys)
  (backend-lw2-legacy
   (declare (dynamic-extent args))
   (multiple-value-bind (query-string cache-key)
       (apply #'%get-posts-index-query-string (list* backend args))
     (if cache-key
	 (get-cached-index-query cache-key query-string)
	 (lw2-graphql-query query-string)))))

(defun get-posts-json ()
  (lw2-graphql-query-noparse (get-posts-index-query-string)))

(defun get-recent-comments (&key with-total)
  (get-cached-index-query "recent-comments" (lw2-query-string :comment :list '((:view . "allRecentComments") (:limit . 20)) (comments-index-fields) :with-total with-total)))

(defun get-recent-comments-json ()
  (lw2-graphql-query-noparse (lw2-query-string :comment :list '((:view . "allRecentComments") (:limit . 20)) (comments-index-fields))))

(defun process-vote-result (res)
  (let ((id (cdr (assoc :--id res)))
	(votetype (cdr (assoc :vote-type (first (cdr (assoc :current-user-votes res)))))))
    (values votetype id)))

(defun process-votes-result (res)
  (loop for v in res
	collect (multiple-value-bind (votetype id) (process-vote-result v) (cons id votetype))))

(defun get-post-vote (post-id auth-token)
  (process-vote-result (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) '(:--id (:current-user-votes :vote-type))) :auth-token auth-token))) 

(define-backend-function get-post-body (post-id &key (revalidate t) force-revalidate auth-token)
  (backend-graphql
   (let ((query-string (lw2-query-string :post :single (alist :document-id post-id) (cons :html-body (posts-index-fields)))))
     (if auth-token
	 (lw2-graphql-query query-string :auth-token auth-token)
	 (lw2-graphql-query-timeout-cached query-string "post-body-json" post-id :revalidate revalidate :force-revalidate force-revalidate)))))

(define-backend-function lw2-query-list-limit-workaround (query-type terms fields &key auth-token)
  (backend-graphql
   (let (items-list)
     (loop for offset from 0 by 500
	as items-next = (lw2-graphql-query (lw2-query-string query-type :list (nconc (alist :limit 500 :offset offset) terms) fields)
					   :auth-token auth-token)
	while items-next
	do (setf items-list (nconc items-list items-next)))
     items-list))
  (backend-accordius
   (lw2-graphql-query (lw2-query-string query-type :list terms fields) :auth-token auth-token)))

(defun get-post-comments-list (post-id view &key auth-token parent-answer-id (fields (post-comments-fields)))
  (let ((terms (alist :view view :post-id post-id)))
    (when parent-answer-id
      (setf terms (acons :parent-answer-id parent-answer-id terms)))
    (lw2-query-list-limit-workaround :comment terms fields :auth-token auth-token)))

(define-backend-function get-post-comments-votes (post-id auth-token)
  (backend-graphql
   (let ((fields '(:--id (:current-user-votes :vote-type))))
     (get-post-comments-list post-id "postCommentsTop" :auth-token auth-token :fields fields)))
  (backend-q-and-a
   (let* ((fields '(:--id (:current-user-votes :vote-type)))
	  (answers (get-post-comments-list post-id "questionAnswers" :auth-token auth-token :fields fields)))
     (process-votes-result
      (nconc
       (get-post-comments-list post-id "postCommentsTop" :auth-token auth-token :fields fields)
       (loop
	  for a in answers
	  nconc (get-post-comments-list post-id "repliesToAnswer" :parent-answer-id (cdr (assoc :--id a)) :auth-token auth-token :fields fields))
       answers)))))

(define-backend-function get-post-comments (post-id &key (revalidate t) force-revalidate)
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

(defun get-post-answers (post-id &key (revalidate t) force-revalidate)
  (let ((fn (lambda ()
	      (let ((answers (get-post-comments-list post-id "questionAnswers")))
		(comments-list-to-graphql-json
		 (append answers
			 (loop
			    for a in answers
			    nconc (get-post-comments-list post-id "repliesToAnswer" :parent-answer-id (cdr (assoc :--id a))))))))))
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
	       (let* ((sequence-json
		       (lw2-graphql-query-noparse
			(lw2-query-string :sequence :single
					  (alist :document-id sequence-id)
					  `(:--id :title :created-at :user-id
						  (:contents :html)
						  (:chapters :title :subtitle :number (:contents :html) (:posts ,@(posts-index-fields)))
						  :grid-image-id :----typename))))
		      (sequence (decode-graphql-json sequence-json))
		      (posts (sequence-post-ids sequence)))
		 (with-cache-transaction
		     (dolist (post-id posts)
		       (let ((old-seqs (if-let (json (cache-get "post-sequence" post-id))
					       (json:decode-json-from-string json))))
			 (unless (member sequence-id old-seqs :test #'string=)
			   (cache-put "post-sequence" post-id (json:encode-json-to-string (cons sequence-id old-seqs)))))))
		 sequence-json))))
     (lw2-graphql-query-timeout-cached fn "sequence-json" sequence-id))))

(define-backend-function get-post-sequences (post-id)
  (backend-graphql
   (if-let (json (cache-get "post-sequence" post-id))
	   (json:decode-json-from-string json))))

(defun preload-sequences-cache ()
  (declare (optimize space (compilation-speed 2) (speed 0)))
  (let ((sequences (apply #'append
			  (loop for view in '("curatedSequences" "communitySequences")
			     collect (lw2-graphql-query (lw2-query-string :sequence :list (alist :view view) '(:--id)))))))
    (dolist (sequence sequences)
      (get-sequence (cdr (assoc :--id sequence))))
    (format t "Retrieved ~A sequences." (length sequences)))
  (values))

(define-backend-function get-user (user-identifier-type user-identifier &key (revalidate t) force-revalidate auth-token)
  (backend-graphql
   (let* ((user-id (ccase user-identifier-type
		     (:user-id user-identifier)
		     (:user-slug (get-slug-userid user-identifier))))
	  (query-string (lw2-query-string :user :single (alist :document-id user-id) (if auth-token (cons :last-notifications-check (user-fields)) (user-fields))))
	  (result (if auth-token
		      (lw2-graphql-query query-string :auth-token auth-token)
		      (lw2-graphql-query-timeout-cached query-string "user-json" user-id :revalidate revalidate :force-revalidate force-revalidate))))
     (alist-bind ((user-id (or simple-string null) :--id)
		  (display-name (or simple-string null))
		  (full-name (or simple-string null))
		  (slug (or simple-string null)))
		 result
		 (when user-id
	 (with-cache-transaction
	   (when display-name
	     (cache-username user-id display-name))
	   (when full-name
	     (cache-user-full-name user-id full-name))
	   (when slug
	     (cache-user-slug user-id slug)
	     (cache-slug-userid slug user-id)))))
     result)))

(define-backend-function get-notifications (&key user-id offset auth-token)
  (backend-lw2-legacy
   (lw2-graphql-query (lw2-query-string :notification :list
					(nconc (alist :user-id user-id :limit 21 :offset offset) *notifications-base-terms*)
					'(:--id :document-type :document-id :link :title :message :type :viewed))
		      :auth-token auth-token))
  (backend-lw2-modernized
   (declare (ignore user-id offset auth-token))
   (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
     (call-next-method))))

(define-backend-function check-notifications (user-id auth-token)
  (backend-lw2-legacy
   (multiple-value-bind (notifications user-info)
       (sb-sys:with-deadline (:seconds 5)
	 (lw2-graphql-query-multi (list
				   (lw2-query-string* :notification :list (nconc (alist :user-id user-id :limit 1) *notifications-base-terms*)
						      '(:created-at))
				   (lw2-query-string* :user :single (alist :document-id user-id) '(:last-notifications-check)))
				  :auth-token auth-token))
     (when (and notifications user-info)
       (local-time:timestamp> (local-time:parse-timestring (cdr (assoc :created-at (first notifications)))) (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info)))))))
  (backend-lw2-modernized
   (declare (ignore user-id auth-token))
   (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
     (call-next-method))))

(define-backend-function get-user-posts (user-id &key offset limit (sort-type :date) drafts auth-token)
  (backend-lw2-legacy
   (declare (special *graphql-correct*))
   (let* ((posts-base-terms
	   (cond
	     (drafts (alist :view "drafts"))
	     ((eq sort-type :score) (alist :view "best"))
	     (t (alist :view "userPosts"))))
	  (posts-base-terms
	   (if (or drafts (boundp '*graphql-correct*))
	       posts-base-terms
	       (cons '(:meta . :null) posts-base-terms))))
     (lw2-graphql-query (lw2-query-string :post :list
					  (nconc (remove nil (alist :offset offset :limit limit :user-id user-id) :key #'cdr) posts-base-terms)
					  (posts-index-fields))
			:auth-token auth-token))))

(define-backend-function get-conversation-messages (conversation-id auth-token)
  (backend-lw2-legacy
   (lw2-graphql-query-multi
    (list
     (lw2-query-string* :conversation :single (alist :document-id conversation-id) '(:title (:participants :display-name :slug)))
     (lw2-query-string* :message :list (alist :view "messagesConversation" :conversation-id conversation-id) *messages-index-fields*))
    :auth-token auth-token)))

(define-backend-function lw2-search-query (query)
  (backend-algolia-search
   (multiple-value-bind (req-stream req-status req-headers req-uri req-reuse-stream want-close)
    (drakma:http-request (algolia-search-uri *current-backend*)
			 :method :post :additional-headers '(("Origin" . "https://www.greaterwrong.com") ("Referer" . "https://www.greaterwrong.com/"))
			 :content (json:encode-json-alist-to-string `(("requests" . ,(loop for index in '("test_posts" "test_comments")
											   collect `(("indexName" . ,index)
												     ("params" . ,(format nil "query=~A&hitsPerPage=20&page=0"
															  (url-rewrite:url-encode query)))))))) 
			 :want-stream t :close t)
    (declare (ignore req-status req-headers req-uri req-reuse-stream))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (unwind-protect
      (values-list (loop for r in (cdr (assoc :results (json:decode-json req-stream)))
			 collect (cdr (assoc :hits r))))
      (if want-close (close req-stream))))))

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
  (simple-cacheable ("post-title" "postid-to-title" post-id)
    (rate-limit (post-id) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) '(:title)))))))) 

(with-rate-limit
  (simple-cacheable ("post-slug" "postid-to-slug" post-id)
    (rate-limit (post-id) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) '(:slug))))))))

(with-rate-limit
  (simple-cacheable ("slug-postid" "slug-to-postid" slug)
    (rate-limit (slug) (cdr (first (lw2-graphql-query (lw2-query-string :post :single (alist :slug slug) '(:--id))))))))

(with-rate-limit
  (simple-cacheable ("username" "userid-to-displayname" user-id)
    (rate-limit (user-id) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) '(:display-name)))))))) 

(with-rate-limit
  (simple-cacheable ("user-slug" "userid-to-slug" user-id)
    (rate-limit (user-id) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) '(:slug))))))))

(with-rate-limit
  (simple-cacheable ("user-full-name" "userid-to-full-name" user-id)
    (rate-limit (user-id) (or (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :document-id user-id) '(:full-name)))))
			      ""))))

(with-rate-limit
  (simple-cacheable ("slug-userid" "slug-to-userid" slug)
    (rate-limit (slug) (cdr (first (lw2-graphql-query (lw2-query-string :user :single (alist :slug slug) '(:--id))))))))

(defun preload-username-cache ()
  (declare (optimize space (compilation-speed 2) (speed 0)))
  (let ((user-list (lw2-graphql-query (lw2-query-string :user :list '() '(:--id :slug :display-name)))))
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
