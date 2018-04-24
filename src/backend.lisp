(uiop:define-package #:lw2.backend
  (:use #:cl #:sb-thread #:flexi-streams #:alexandria #:lw2-viewer.config #:lw2.lmdb #:lw2.utils #:lw2.hash-utils)
  (:export #:*posts-index-fields* #:*comments-index-fields* #:*messages-index-fields*
           #:*notifications-base-terms*
	   #:log-condition #:log-conditions #:start-background-loader #:stop-background-loader #:background-loader-running-p
	   #:lw2-graphql-query-streamparse #:lw2-graphql-query-noparse #:decode-graphql-json #:lw2-graphql-query #:graphql-query-string* #:graphql-query-string #:lw2-graphql-query-map #:lw2-graphql-query-multi
	   #:make-posts-list-query #:get-posts #:get-posts-json #:get-post-body #:get-post-vote #:get-post-comments #:get-post-comments-votes #:get-recent-comments #:get-recent-comments-json
	   #:lw2-search-query #:get-post-title #:get-post-slug #:get-slug-postid #:get-username #:get-user-slug)
  (:recycle #:lw2-viewer))

(in-package #:lw2.backend)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defparameter *posts-index-fields* '(:title :--id :slug :user-id :posted-at :base-score :comment-count :page-url :url))
(defparameter *comments-index-fields* '(:--id :user-id :post-id :posted-at :parent-comment-id (:parent-comment :--id :user-id :post-id) :base-score :page-url :html-body)) 
(defparameter *messages-index-fields* '(:--id :user-id :created-at :content (:conversation :--id :title) :----typename))

(defparameter *notifications-base-terms* (alist :view "userNotifications" :created-at :null :viewed :null))

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

(defun background-loader ()
  (loop
    (handler-case
      (log-conditions 
	(let ((posts-json (sb-ext:with-timeout 120 (get-posts-json))))
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
	(let ((recent-comments-json (sb-ext:with-timeout 120 (get-recent-comments-json))))
	  (if (and recent-comments-json (ignore-errors (json:decode-json-from-string recent-comments-json)))
	    (cache-put "index-json" "recent-comments" recent-comments-json))))
      (t (condition) (values nil condition)))
    (if (wait-on-semaphore *background-loader-semaphore* :timeout 60)
        (return))))

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
        (setf *background-loader-thread* nil))
      (warn "Background loader not running.")))

(defun lw2-graphql-query-streamparse (query &key auth-token)
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

(defun decode-graphql-json (json-string)
  (let* ((decoded (json:decode-json-from-string json-string))
	 (errors (cadr (assoc :errors decoded)))
	 (data (cdadr (assoc :data decoded))))
    (if errors
      (let ((message (cdr (assoc :message errors)))
            (path (cdr (assoc :path errors))))
        (if (and path (> (length path) 1))
            (values data errors)
            (cond
              ((search "document_not_found" message) (error "LW2 server reports: document not found."))
              ((search "not_allowed" message) (error "LW2 server reports: not allowed."))
              (t (error "LW2 server reports: ~A" message)))))
      data)))  

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
                     do (format stream "g~A:~A " n q))
               (format stream "}")))
           (result (lw2-graphql-query-streamparse query-string :auth-token auth-token)))
      (values
        (loop as results = (cdr (assoc :data result)) then (if passthrough-p results (rest results))
              for (out passthrough-p) in map-values
              as result-data-cell = (first results)
              as result-data = (if passthrough-p out (cdr result-data-cell))
              for input-data in data
              collect (if postprocess (funcall postprocess input-data result-data) result-data))
        (cdr (assoc :errors result))))))

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
(defparameter *cache-stale-factor* 100)

(defun cache-is-fresh (cache-db key)
  (let ((metadata (if-let (m-str (cache-get (format nil "~A-meta" cache-db) key)) (read-from-string m-str)))
        (current-time (get-unix-time)))
    (if-let ((last-mod (cdr (assoc :last-modified metadata)))
             (last-checked (cdr (assoc :last-checked metadata))))
            (> (- last-checked last-mod) (* *cache-stale-factor* (- current-time last-checked))))))

(defun ensure-cache-update-thread (query cache-db cache-key)
  (let ((key (format nil "~A-~A" cache-db cache-key))) 
    (labels ((background-fn ()
			    (handler-case 
			      (prog1
				(cache-update cache-db cache-key (lw2-graphql-query-noparse query))
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
    (multiple-value-bind (cached-result is-fresh) (with-cache-transaction (values (cache-get cache-db cache-key) (cache-is-fresh cache-db cache-key)))
      (if (and cached-result (if force-revalidate (not revalidate) (or is-fresh (not revalidate))))
          (decode-graphql-json cached-result)
          (let ((timeout (if cached-result (if force-revalidate nil 3) nil))
                (thread (ensure-cache-update-thread query cache-db cache-key)))
            (decode-graphql-json
              (handler-case
                (sb-thread:join-thread thread :timeout timeout)
                (t () (or cached-result
                          (error "Failed to load ~A ~A and no cached version available." cache-db cache-key)))))))))

(defun graphql-query-string* (query-type terms fields)
  (labels ((terms (tlist)
		  (loop for (k . v) in tlist
			when k
			collect (format nil "~A:~A"
					(json:lisp-to-camel-case (string k))
					(typecase v
					  ((member t) "true") 
					  ((member nil) "false")
					  ((member :null) "null")
					  ((member :undefined) "undefined")
					  (list (format nil "{~{~A~^,~}}" (terms v)))
					  (t (format nil "~S" v))))))
	   (fields (flist)
		   (map 'list (lambda (x) (typecase x
					    (string x)
					    (symbol (json:lisp-to-camel-case (string x)))
					    (list (format nil "~A{~{~A~^,~}}" (json:lisp-to-camel-case (string (first x))) (fields (rest x))))))
			flist)))
    (format nil "~A(~{~A~^,~})~@[{~{~A~^,~}}~]"
	    query-type
	    (terms terms)
	    (fields fields))))

(defun graphql-query-string (query-type terms fields)
  (format nil "{~A}" (graphql-query-string* query-type terms fields)))

(declaim (inline make-posts-list-query)) 
(defun make-posts-list-query (&key (view "frontpage-rss") (limit 20) (meta nil) (before nil) (after nil) (with-body nil))
  (declare (type string view)
	   (type (integer 1) limit)
	   (type boolean meta)
	   (type (or string null) before after))
  (format nil "{PostsList (terms:{view:\"~A\",limit:~A,meta:~A~A~A}) {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url~A}}"
	  view
	  limit
	  (if meta "true" "false")
	  (if before (format nil ",before:\"~A\"" before) "")
	  (if after (format nil ",after:\"~A\"" after) "")
	  (if with-body ", htmlBody" ""))) 

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

(defun get-posts ()
  (get-cached-index-query "new-not-meta" (make-posts-list-query)))

(defun get-posts-json ()
  (lw2-graphql-query-noparse (make-posts-list-query)))

(defun get-recent-comments ()
  (get-cached-index-query "recent-comments" (graphql-query-string "CommentsList" '((:terms . ((:view . "recentComments") (:limit . 20)))) *comments-index-fields*)))

(defun get-recent-comments-json ()
  (lw2-graphql-query-noparse (graphql-query-string "CommentsList" '((:terms . ((:view . "recentComments") (:limit . 20)))) *comments-index-fields*)))

(defun process-vote-result (res)
  (let ((id (cdr (assoc :--id res)))
	(votetype (cdr (assoc :vote-type (first (cdr (assoc :current-user-votes res)))))))
    (values votetype id)))

(defun process-votes-result (res)
  (loop for v in res
	collect (multiple-value-bind (votetype id) (process-vote-result v) (cons id votetype))))

(defun get-post-vote (post-id auth-token)
  (process-vote-result (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {_id, currentUserVotes{voteType}}}" post-id) :auth-token auth-token))) 

(defun get-post-body (post-id &key (revalidate t) force-revalidate auth-token)
  (let ((query-string (format nil "{PostsSingle(documentId:\"~A\") {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url, frontpageDate, meta, draft, htmlBody}}" post-id)))
    (if auth-token
        (lw2-graphql-query query-string :auth-token auth-token)
        (lw2-graphql-query-timeout-cached query-string "post-body-json" post-id :revalidate revalidate :force-revalidate force-revalidate))))

(defun get-post-comments-votes (post-id auth-token)
  (process-votes-result (lw2-graphql-query (format nil "{CommentsList(terms:{view:\"postCommentsTop\",limit:10000,postId:\"~A\"}) {_id, currentUserVotes{voteType}}}" post-id) :auth-token auth-token)))

(defun get-post-comments (post-id &key (revalidate t) force-revalidate)
  (lw2-graphql-query-timeout-cached (format nil "{CommentsList(terms:{view:\"postCommentsTop\",limit:10000,postId:\"~A\"}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}" post-id) "post-comments-json" post-id :revalidate revalidate :force-revalidate force-revalidate))

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

(simple-cacheable ("post-title" "postid-to-title" post-id)
  (cdr (first (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {title}}" post-id))))) 

(simple-cacheable ("post-slug" "postid-to-slug" post-id)
  (cdr (first (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {slug}}" post-id)))))

(simple-cacheable ("slug-postid" "slug-to-postid" slug)
  (cdr (first (lw2-graphql-query (format nil "{PostsSingle(slug:\"~A\") {_id}}" slug)))))

(simple-cacheable ("username" "userid-to-displayname" user-id)
  (cdr (first (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {displayName}}" user-id))))) 

(simple-cacheable ("user-slug" "userid-to-slug" user-id)
  (cdr (first (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {slug}}" user-id)))))

(defun preload-username-cache ()
  (let ((user-list (lw2-graphql-query "{UsersList(terms:{}) {_id, displayName}}")))
    (loop for user in user-list
	  do (cache-username (cdr (assoc :--id user)) (cdr (assoc :display-name user)))))) 
