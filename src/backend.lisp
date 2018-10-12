(uiop:define-package #:lw2.backend
  (:use #:cl #:sb-thread #:flexi-streams #:alexandria #:lw2-viewer.config #:lw2.graphql #:lw2.lmdb #:lw2.utils #:lw2.hash-utils)
  (:export #:*graphql-debug-output*
           #:*posts-index-fields* #:*comments-index-fields* #:*messages-index-fields*
           #:*notifications-base-terms*
           #:backend-base #:backend-lw2-legacy #:backend-lw2-modernized #:backend-lw2 #:backend-accordius
           #:*current-backend*
           #:declare-backend-function #:define-backend-operation
           #:condition-http-return-code
           #:lw2-error #:lw2-client-error #:lw2-not-found-error #:lw2-user-not-found-error #:lw2-not-allowed-error #:lw2-server-error #:lw2-connection-error #:lw2-unknown-error
	   #:log-condition #:log-conditions #:start-background-loader #:stop-background-loader #:background-loader-running-p
	   #:lw2-graphql-query-streamparse #:lw2-graphql-query-noparse #:decode-graphql-json #:lw2-graphql-query
           #:lw2-query-string* #:lw2-query-string
           #:lw2-graphql-query-map #:lw2-graphql-query-multi
	   #:get-posts-index #:get-posts-json #:get-post-body #:get-post-vote #:get-post-comments #:get-post-comments-votes #:get-recent-comments #:get-recent-comments-json
           #:get-notifications #:check-notifications
	   #:lw2-search-query #:get-post-title #:get-post-slug #:get-slug-postid #:get-username #:get-user-slug)
  (:recycle #:lw2-viewer)
  (:unintern #:get-posts #:make-posts-list-query))

(in-package #:lw2.backend)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defvar *graphql-debug-output* nil)

(defparameter *posts-index-fields* '(:title :--id :slug :user-id :posted-at :base-score :comment-count :page-url :url :word-count :frontpage-date :curated-date :meta :draft :af :vote-count))
(defparameter *comments-index-fields* '(:--id :user-id :post-id :posted-at :parent-comment-id (:parent-comment :--id :user-id :post-id) :base-score :page-url :vote-count :html-body))
(defparameter *messages-index-fields* '(:--id :user-id :created-at :content (:conversation :--id :title) :----typename))

(defparameter *notifications-base-terms* (alist :view "userNotifications" :created-at :null :viewed :null))

(defclass backend-base () ())

(defclass backend-lw2-legacy (backend-base) ())

(defclass backend-lw2-modernized (backend-base) ())

(defclass backend-lw2 (backend-lw2-modernized backend-lw2-legacy) ())

(defclass backend-accordius (backend-lw2-modernized backend-lw2-legacy) ())

(defparameter *current-backend* (make-instance (symbolicate "BACKEND-" (string-upcase *backend-type*))))

(defmacro declare-backend-function (name)
  (let ((inner-name (symbolicate "%" name)))
   `(progn
      (export '(,name ,inner-name))
      (defmacro ,name (&rest args) (list* ',inner-name '*current-backend*  args)))))

(defmacro define-backend-operation (name backend &rest args)
  (let* ((inner-name (symbolicate "%" name))
         (latter-args (member-if #'listp args))
         (method-qualifiers (ldiff args latter-args))
         (method-args (first latter-args))
         (body (rest latter-args)))
    `(defmethod ,inner-name ,.method-qualifiers ((backend ,backend) ,@method-args) ,@body)))

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

(defvar *background-loader-thread* nil)
(defvar *background-loader-semaphore* (make-semaphore :count 1))

(defun background-loader-running-p ()
  (case (semaphore-count *background-loader-semaphore*)
    (0 t)
    (1 nil)))

(declare-backend-function comments-list-to-graphql-json)

(define-backend-operation comments-list-to-graphql-json backend-lw2-legacy (comments-list)
  (json:encode-json-to-string
    (json:with-local-class-registry ()
      (json:make-object `((data . ,(json:make-object `((*comments-list . ,comments-list)) nil))) nil))))

(define-backend-operation comments-list-to-graphql-json backend-lw2 (comments-list)
  (json:encode-json-to-string
    (json:with-local-class-registry ()
      (json:make-object `((data . ,(json:make-object `((*comments-list . ,(json:make-object `((results . ,comments-list)) nil))) nil))) nil))))

(defun background-loader ()
  (let (last-comment-processed)
    (loop
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
                            if (string= comment-id last-comment-processed) return nil
                            do
                            (with-cache-transaction
                              (let* ((post-id (cdr (assoc :post-id comment)))
                                     (post-comments (ignore-errors (decode-graphql-json (cache-get "post-comments-json" post-id))))
                                     (new-post-comments (sort (cons comment (delete-if (lambda (c) (string= comment-id (cdr (assoc :--id c)))) post-comments))
                                                              #'> :key (lambda (c) (cdr (assoc :base-score c))))))
                                (cache-update "post-comments-json" post-id (comments-list-to-graphql-json new-post-comments)))))
                      (setf last-comment-processed (cdr (assoc :--id (first recent-comments)))))))
        (t (condition) (values nil condition)))
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
        (setf *background-loader-thread* nil)
        (signal-semaphore *background-loader-semaphore*))
      (warn "Background loader not running.")))

(defun do-graphql-debug (query)
  (when *graphql-debug-output*
    (format *graphql-debug-output* "~&GraphQL query: ~A~%" query)))

(defun lw2-graphql-query-streamparse (query &key auth-token)
  (do-graphql-debug query)
  (multiple-value-bind (req-stream status-code headers final-uri reuse-stream want-close)
    (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
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
    (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
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

(declare-backend-function fixup-lw2-return-value)

(define-backend-operation fixup-lw2-return-value backend-lw2-legacy (value)
  value)

(define-backend-operation fixup-lw2-return-value backend-lw2 (value)
  (let ((junk (caar value)))
    (if (member junk '(:result :results :total-count))
        (cdar value)
        value)))

(defun decode-graphql-json (json-string)
  (let* ((decoded (json:decode-json-from-string json-string))
	 (errors (cdr (assoc :errors decoded)))
	 (data (fixup-lw2-return-value (cdadr (assoc :data decoded)))))
    (signal-lw2-errors errors)
    data))

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

(defun ensure-cache-update-thread (query cache-db cache-key)
  (let ((key (format nil "~A-~A" cache-db cache-key))) 
    (labels ((background-fn ()
			    (handler-case 
			      (prog1
				(cache-update cache-db cache-key (run-query query))
				(remhash key *background-cache-update-threads*))
			      (t (c)
				 (remhash key *background-cache-update-threads*)
				 (log-condition c)
				 (sb-thread:abort-thread))))) 
      (sb-ext:with-locked-hash-table (*background-cache-update-threads*)
				     (let ((thread (gethash key *background-cache-update-threads*)))
				       (if thread thread
					 (setf (gethash key *background-cache-update-threads*)
					       (sb-thread:make-thread #'background-fn)))))))) 

(defun lw2-graphql-query-timeout-cached (query cache-db cache-key &key (revalidate t) force-revalidate)
    (multiple-value-bind (cached-result is-fresh) (with-cache-readonly-transaction (values (cache-get cache-db cache-key) (cache-is-fresh cache-db cache-key)))
      (if (and cached-result (if force-revalidate (not revalidate) (or is-fresh (not revalidate))))
          (decode-graphql-json cached-result)
          (let ((timeout (if cached-result (if force-revalidate nil 3) nil))
                (thread (ensure-cache-update-thread query cache-db cache-key)))
            (decode-graphql-json
              (handler-case
                (sb-thread:join-thread thread :timeout timeout)
                (t () (or cached-result
                          (error "Failed to load ~A ~A and no cached version available." cache-db cache-key)))))))))

(declare-backend-function lw2-query-string*)

(define-backend-operation lw2-query-string* backend-lw2-legacy (query-type return-type args fields)
  (graphql-query-string*
    (concatenate 'string (string-capitalize query-type)
                         "s"
                         (string-capitalize return-type))
    (if (eq return-type :single)
        args
        (alist :terms args))
    fields))

(define-backend-operation lw2-query-string* backend-lw2 (query-type return-type args fields)
  (graphql-query-string*
    (if (eq return-type :single)
        (string-downcase query-type)
        (concatenate 'string (string-downcase query-type) "s"))
    (alist :input (if (eq return-type :single)
                      (alist :selector args)
                      (alist :terms args)))
    (case return-type
        (:total '(:total-count))
        (:list (list (cons :results fields)))
        (:single (list (cons :result fields))))))

(declare-backend-function lw2-query-string)

(define-backend-operation lw2-query-string backend-lw2-legacy (query-type return-type args fields)
  (format nil "{~A}" (lw2-query-string* query-type return-type args fields)))

(defun get-cached-index-query (cache-id query)
  (labels ((query-and-put ()
             (let* ((result (lw2-graphql-query-noparse query))
                    (decoded-result (decode-graphql-json result)))
               (cache-put "index-json" cache-id result)
               decoded-result)))
    (let ((cached-result (cache-get "index-json" cache-id)))
      (if (and cached-result (background-loader-running-p))
        (decode-graphql-json cached-result)
        (if cached-result
            (handler-case
              (query-and-put)
              (t () (decode-graphql-json cached-result)))
            (query-and-put))))))

(declare-backend-function get-posts-index-query-string)

(define-backend-operation get-posts-index-query-string backend-lw2-legacy (&key view sort (limit 20) offset before after)
  (multiple-value-bind (view-terms cache-key)
    (alexandria:switch (view :test #'string=)
                       ("featured" (alist :view "curated"))
                       ("new" (alist :view (if (string= sort "hot") "community" "community-rss")))
                       ("meta" (alist :view "new" :meta t :all t))
                       ("alignment-forum" (alist :view "new" :af t))
                       (t (values (alist :view (if (string= sort "hot") "magicalSorting" "frontpage-rss")) (if (not (or sort (/= limit 20) offset before after)) "new-not-meta"))))
    (let* ((extra-terms
             (remove-if (lambda (x) (null (cdr x)))
                        (alist :before before :after after :limit limit :offset offset)))
           (query-string (lw2-query-string :post :list (nconc view-terms extra-terms) *posts-index-fields*)))
      (values query-string cache-key))))

(declare-backend-function get-posts-index)

(define-backend-operation get-posts-index backend-lw2-legacy (&rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (multiple-value-bind (query-string cache-key)
    (apply #'%get-posts-index-query-string (list* backend args))
    (if cache-key
        (get-cached-index-query cache-key query-string)
        (lw2-graphql-query query-string))))

(defun get-posts-json ()
  (lw2-graphql-query-noparse (get-posts-index-query-string)))

(defun get-recent-comments ()
  (get-cached-index-query "recent-comments" (lw2-query-string :comment :list '((:view . "recentComments") (:limit . 20)) *comments-index-fields*)))

(defun get-recent-comments-json ()
  (lw2-graphql-query-noparse (lw2-query-string :comment :list '((:view . "recentComments") (:limit . 20)) *comments-index-fields*)))

(defun process-vote-result (res)
  (let ((id (cdr (assoc :--id res)))
	(votetype (cdr (assoc :vote-type (first (cdr (assoc :current-user-votes res)))))))
    (values votetype id)))

(defun process-votes-result (res)
  (loop for v in res
	collect (multiple-value-bind (votetype id) (process-vote-result v) (cons id votetype))))

(defun get-post-vote (post-id auth-token)
  (process-vote-result (lw2-graphql-query (lw2-query-string :post :single (alist :document-id post-id) '(:--id (:current-user-votes :vote-type))) :auth-token auth-token))) 

(defun get-post-body (post-id &key (revalidate t) force-revalidate auth-token)
  (let ((query-string (lw2-query-string :post :single (alist :document-id post-id) (cons :html-body *posts-index-fields*))))
    (if auth-token
        (lw2-graphql-query query-string :auth-token auth-token)
        (lw2-graphql-query-timeout-cached query-string "post-body-json" post-id :revalidate revalidate :force-revalidate force-revalidate))))

(defun get-post-comments-votes (post-id auth-token)
  (process-votes-result (lw2-graphql-query (lw2-query-string :comment :list (alist :view "postCommentsTop" :limit 10000 :post-id post-id) '(:--id (:current-user-votes :vote-type))) :auth-token auth-token)))

(defun get-post-comments (post-id &key (revalidate t) force-revalidate)
  (let ((fn (lambda ()
              (let ((base-terms (alist :view "postCommentsTop" :post-id post-id))
                    (comments-fields '(:--id :user-id :post-id :posted-at :parent-comment-id :base-score :page-url :vote-count :html-body)))
                (multiple-value-bind (comments-total comments-list)
                  (lw2-graphql-query-multi (list (lw2-query-string* :comment :total base-terms nil)
                                                 (lw2-query-string* :comment :list (nconc (alist :limit 500) base-terms) comments-fields)))
                  (loop for offset from 500 to comments-total by 500
                        as comments-next = (lw2-graphql-query (lw2-query-string :comment :list (nconc (alist :limit 500 :offset offset) base-terms) comments-fields))
                        do (setf comments-list (nconc comments-list comments-next)))
                  (comments-list-to-graphql-json comments-list))))))
    (lw2-graphql-query-timeout-cached fn "post-comments-json" post-id :revalidate revalidate :force-revalidate force-revalidate)))

(declare-backend-function get-notifications)

(define-backend-operation get-notifications backend-lw2-legacy (&key user-id offset auth-token)
  (lw2-graphql-query (lw2-query-string :notification :list
                                       (nconc (alist :user-id user-id :limit 21 :offset offset) *notifications-base-terms*)
                                       '(:--id :document-type :document-id :link :title :message :type :viewed))
                     :auth-token auth-token))

(declare-backend-function check-notifications)

(define-backend-operation check-notifications backend-lw2-legacy (user-id auth-token)
  (multiple-value-bind (notifications user-info)
    (sb-sys:with-deadline (:seconds 5)
                          (lw2-graphql-query-multi (list
                                                     (lw2-query-string* :notification :list (nconc (alist :user-id user-id :limit 1) *notifications-base-terms*)
                                                                        '(:created-at))
                                                     (lw2-query-string* :user :single (alist :document-id user-id) '(:last-notifications-check)))
                                                   :auth-token auth-token))
    (when (and notifications user-info)
      (local-time:timestamp> (local-time:parse-timestring (cdr (assoc :created-at (first notifications)))) (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info)))))))

(define-backend-operation get-notifications backend-lw2-modernized (&key user-id offset auth-token)
                          (declare (ignore user-id offset auth-token))
                          (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
                            (call-next-method)))

(define-backend-operation check-notifications backend-lw2-modernized (user-id auth-token)
                          (declare (ignore user-id auth-token))
                          (let ((*notifications-base-terms* (remove :null *notifications-base-terms* :key #'cdr)))
                            (call-next-method)))

(declare-backend-function get-user-posts)

(define-backend-operation get-user-posts backend-lw2-legacy (user-id &key offset limit (sort-type :date) drafts auth-token)
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
                                         *posts-index-fields*)
                       :auth-token auth-token)))

(define-backend-operation get-user-posts backend-accordius (user-id &key offset limit (sort-type :date) drafts auth-token)
  (declare (ignore user-id offset limit sort-type drafts auth-token))
  (let ((*graphql-correct* t))
    (declare (special *graphql-correct*))
    (call-next-method)))

(declare-backend-function get-conversation-messages)

(define-backend-operation get-conversation-messages backend-lw2-legacy (conversation-id auth-token)
  (lw2-graphql-query-multi
    (list
      (lw2-query-string* :conversation :single (alist :document-id conversation-id) '(:title (:participants :display-name :slug)))
      (lw2-query-string* :message :list (alist :view "messagesConversation" :conversation-id conversation-id) *messages-index-fields*))
    :auth-token (hunchentoot:cookie-in "lw2-auth-token")))

(define-backend-operation get-conversation-messages backend-lw2 (conversation-id auth-token)
  (declare (ignore conversation-id auth-token))
  (let ((*messages-index-fields* (cons :html-body *messages-index-fields*)))
    (call-next-method)))

(define-backend-operation get-conversation-messages backend-accordius (conversation-id auth-token)
  (declare (ignore conversation-id auth-token))
  (let ((*messages-index-fields* (cons :html-body (remove :content *messages-index-fields*))))
    (call-next-method)))

(defun lw2-search-query (query)
  (multiple-value-bind (req-stream req-status req-headers req-uri req-reuse-stream want-close)
    (drakma:http-request "https://z0gr6exqhd-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20vanilla%20JavaScript%203.24.5%3Breact-instantsearch%204.1.3%3BJS%20Helper%202.23.0&x-algolia-application-id=Z0GR6EXQHD&x-algolia-api-key=0b1d20b957917dbb5e1c2f3ad1d04ee2"
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
      (if want-close (close req-stream)))))

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

(defun preload-username-cache ()
  (let ((user-list (lw2-graphql-query (lw2-query-string :user :list '() '(:--id :display-name)))))
    (loop for user in user-list
	  do (cache-username (cdr (assoc :--id user)) (cdr (assoc :display-name user)))))) 
