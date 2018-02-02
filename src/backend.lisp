(defpackage #:lw2.backend
  (:use #:cl #:sb-thread #:flexi-streams #:lw2-viewer.config #:lw2.lmdb)
  (:export #:log-condition #:log-conditions #:start-background-loader #:stop-background-loader
	   #:lw2-graphql-query-streamparse #:lw2-graphql-query-noparse #:decode-graphql-json #:lw2-graphql-query #:graphql-query-string #:make-posts-list-query
	   #:get-posts #:get-posts-json #:get-post-body #:get-post-vote #:get-post-comments #:get-post-comments-votes #:get-recent-comments #:get-recent-comments-json
	   #:lw2-search-query #:get-post-title #:get-post-slug #:get-username #:get-user-slug))

(in-package #:lw2.backend)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defvar *background-loader-thread* nil) 

(defun log-condition (condition)
  (with-open-file (outstream "./logs/error.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format outstream "~%~A: ~S ~A~%" (local-time:format-timestring nil (local-time:now)) condition condition)
    (sb-debug:print-backtrace :stream outstream :from :interrupted-frame :print-frame-source t))) 

(defmacro log-conditions (&body body)
  `(block log-conditions
     (handler-bind
       (((or warning serious-condition) (lambda (c) (log-condition c))))
       (progn ,@body))))

(defun background-loader ()
  (loop
    (ignore-errors 
      (log-conditions 
	(let ((posts-json (get-posts-json)))
	  (when (and posts-json (ignore-errors (json:decode-json-from-string posts-json)))
	    (cache-put "index-json" "new-not-meta" posts-json)
	    (let ((posts-list (decode-graphql-json posts-json))) 
	      (with-db (db "postid-to-title")
		       (dolist (post posts-list)
			 (lmdb-put-string db (cdr (assoc :--id post)) (cdr (assoc :title post)))))
	      (with-db (db "postid-to-slug")
		       (dolist (post posts-list)
			 (lmdb-put-string db (cdr (assoc :--id post)) (cdr (assoc :slug post))))))))))
    (ignore-errors
      (log-conditions
	(let ((recent-comments-json (get-recent-comments-json)))
	  (if (and recent-comments-json (ignore-errors (json:decode-json-from-string recent-comments-json)))
	    (cache-put "index-json" "recent-comments" recent-comments-json))))) 
    (sleep 60))) 

(defun start-background-loader ()
  (assert (not *background-loader-thread*))
  (setf *background-loader-thread* (sb-thread:make-thread #'background-loader))) 

(defun stop-background-loader ()
  (with-cache-mutex
    (sb-thread:terminate-thread *background-loader-thread*))
  (setf *background-loader-thread* nil)) 

(defun lw2-graphql-query-streamparse (query &key auth-token)
  (multiple-value-bind (req-stream status-code headers final-uri reuse-stream want-close)
    (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil)
			 :want-stream t :close t)
    (declare (ignore status-code headers final-uri reuse-stream))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (unwind-protect
      (rest (cadar (json:decode-json req-stream)))
      (if want-close (close req-stream)))))

(defun lw2-graphql-query-noparse (query &key auth-token)
    (octets-to-string (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil)
			 :want-stream nil :close t)
		      :external-format :utf-8)) 

(defun decode-graphql-json (json-string)
  (let* ((decoded (json:decode-json-from-string json-string))
	 (errors (cadr (assoc :errors decoded)))
	 (data (cdadr (assoc :data decoded))))
    (if errors
      (let ((message (cdr (assoc :message errors))))
	(cond 
	  ((search "document_not_found" message) (error "LW2 server reports: document not found."))
	  ((search "not_allowed" message) (error "LW2 server reports: not allowed."))
	  (t (error "LW2 server reports: ~A" message))))
      data)))  

(defun lw2-graphql-query (query &key auth-token)
  (decode-graphql-json (lw2-graphql-query-noparse query :auth-token auth-token))) 

(defvar *background-cache-update-threads* (make-hash-table :test 'equal
							   :weakness :value
							   :synchronized t)) 
 
(defun ensure-cache-update-thread (query cache-db cache-key)
  (let ((key (format nil "~A-~A" cache-db cache-key))) 
    (labels ((background-fn ()
			    (handler-case 
			      (prog1
				(cache-put cache-db cache-key (lw2-graphql-query-noparse query))
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

(defun lw2-graphql-query-timeout-cached (query cache-db cache-key &key (revalidate t))
  (let ((cached-result (cache-get cache-db cache-key))) 
    (if (and cached-result (not revalidate))
      (decode-graphql-json cached-result)
      (let ((timeout (if cached-result 3 nil)) 
	    (thread (ensure-cache-update-thread query cache-db cache-key))) 
	(decode-graphql-json
	  (handler-case
	    (sb-thread:join-thread thread :timeout timeout)
	    (t () (or cached-result
		      (error "Failed to load ~A ~A and no cached version available." cache-db cache-key)))))))))

(defun graphql-query-string (query-type terms fields)
  (format nil "{~A(~{~A~^,~}){~{~A~^,~}}}"
	  query-type
	  (labels ((terms (tlist)
			  (loop for (k . v) in tlist
				when k
				collect (format nil "~A:~A"
						(json:lisp-to-camel-case (string k))
						(typecase v
						  ((member t) "true") 
						  ((member nil) "false")
						  (list (format nil "{~{~A~^,~}}" (terms v)))
						  (t (format nil "~S" v)))))))
	    (terms terms))
	  (map 'list (lambda (x) (json:lisp-to-camel-case (string x))) fields)))

(declaim (inline make-posts-list-query)) 
(defun make-posts-list-query (&key (view "frontpage") (limit 20) (meta nil) (before nil) (after nil) (with-body nil))
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

(defun get-posts ()
  (let ((cached-result (and *background-loader-thread* (cache-get "index-json" "new-not-meta")))) 
    (if cached-result
      (decode-graphql-json cached-result)
      (lw2-graphql-query (make-posts-list-query)))))

(defun process-vote-result (res)
  (let ((id (cdr (assoc :--id res)))
	(votetype (cdr (assoc :vote-type (first (cdr (assoc :current-user-votes res)))))))
    (values votetype id)))

(defun process-votes-result (res)
  (loop for v in res
	collect (multiple-value-bind (votetype id) (process-vote-result v) (cons id votetype))))

(defun get-posts-json ()
  (lw2-graphql-query-noparse (make-posts-list-query)))

(defun get-post-vote (post-id auth-token)
  (process-vote-result (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {_id, currentUserVotes{voteType}}}" post-id) :auth-token auth-token))) 

(defun get-post-body (post-id &key (revalidate t))
  (lw2-graphql-query-timeout-cached (format nil "{PostsSingle(documentId:\"~A\") {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url, frontpageDate, meta, draft, htmlBody}}" post-id) "post-body-json" post-id :revalidate revalidate))

(defun get-post-comments-votes (post-id auth-token)
  (process-votes-result (lw2-graphql-query (format nil "{CommentsList(terms:{view:\"postCommentsTop\",limit:10000,postId:\"~A\"}) {_id, currentUserVotes{voteType}}}" post-id) :auth-token auth-token)))

(defun get-post-comments (post-id)
  (lw2-graphql-query-timeout-cached (format nil "{CommentsList(terms:{view:\"postCommentsTop\",limit:10000,postId:\"~A\"}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}" post-id) "post-comments-json" post-id))

(defun get-recent-comments ()
  (let ((cached-result (and *background-loader-thread* (cache-get "index-json" "recent-comments"))))
    (if cached-result
      (rest (cadar (json:decode-json-from-string cached-result))) 
      (lw2-graphql-query (format nil "{CommentsList (terms:{view:\"postCommentsNew\",limit:20}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}")))))

(defun get-recent-comments-json ()
  (lw2-graphql-query-noparse (format nil "{CommentsList (terms:{view:\"postCommentsNew\",limit:20}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}")))

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

(simple-cacheable ("username" "userid-to-displayname" user-id)
  (cdr (first (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {displayName}}" user-id))))) 

(simple-cacheable ("user-slug" "userid-to-slug" user-id)
  (cdr (first (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {slug}}" user-id)))))

(defun preload-username-cache ()
  (let ((user-list (lw2-graphql-query "{UsersList(terms:{}) {_id, displayName}}")))
    (loop for user in user-list
	  do (cache-username (cdr (assoc :--id user)) (cdr (assoc :display-name user)))))) 
