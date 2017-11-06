(defpackage lw2-viewer
  (:use #:cl #:flexi-streams))

(in-package #:lw2-viewer) 

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))
;(defvar *lw2-stream* nil) 
(defvar *username-cache* (make-hash-table :test 'equal)) 

(defparameter *cache-db* "./cache/") 
(defvar *db-mutex* (sb-thread:make-mutex :name "lmdb")) 

(defmacro with-db ((db) &body body)
  (alexandria:with-gensyms (env txn)
			   `(sb-thread:with-mutex (*db-mutex*)
						  (let ((,env (lmdb:make-environment *cache-db*)))
						    (lmdb:with-environment (,env)
									   (let ((,txn (lmdb:make-transaction ,env)))
									     (lmdb:begin-transaction ,txn)
									     (let ((db (lmdb:make-database ,txn ,db)))
									       (lmdb:with-database (,db)
												   (prog1
												     (progn
												       ,@body)
												     (lmdb:commit-transaction ,txn))))))))))

(defun cache-put (db key value)
  (with-db (db) 
	   (if (lmdb:put db (string-to-octets key :external-format :utf-8)
			 (string-to-octets value :external-format :utf-8))
	     value
	     nil))) 

(defun cache-get (db key)
  (with-db (db) 
	   (let ((result (lmdb:get db (string-to-octets key :external-format :utf-8)))) 
	     (if result
	       (octets-to-string result :external-format :utf-8)
	       nil)))) 

(defvar *background-loader-thread* nil) 

(defun lw2-graphql-query-streamparse (query)
  (multiple-value-bind (req-stream status-code headers final-uri reuse-stream)
    (drakma:http-request "https://www.lesserwrong.com/graphql" :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :want-stream t)
    (declare (ignore status-code headers final-uri reuse-stream))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (rest (cadar (json:decode-json req-stream))))) 

(defun lw2-graphql-query-noparse (query)
    (octets-to-string (drakma:http-request "https://www.lesserwrong.com/graphql" :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :want-stream nil)
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

(defun lw2-graphql-query (query)
  (decode-graphql-json (lw2-graphql-query-noparse query))) 

(defun lw2-graphql-query-timeout-cached (query cache-db cache-key)
  (let* ((cached-result (cache-get cache-db cache-key))
	 (new-result 
	   (if cached-result
	     (let ((thread (sb-thread:make-thread
			     (lambda () (cache-put cache-db cache-key (lw2-graphql-query-noparse query))))))
	       (handler-case
		 (sb-thread:join-thread thread :timeout 1)
		 (t () cached-result)))
	     (cache-put cache-db cache-key (lw2-graphql-query-noparse query)))))
    (decode-graphql-json new-result)))

(declaim (inline make-posts-list-query)) 
(defun make-posts-list-query (&key (view "new") (limit 20) (meta nil) (frontpage t) (before nil) (after nil))
  (declare (type string view)
	   (type (integer 1) limit)
	   (type boolean meta)
	   (type (or string null) before after))
  (format nil "{PostsList (terms:{view:\"~A\",limit:~A,meta:~A,frontpage:~A~A~A}) {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url}}"
	  view
	  limit
	  (if meta "true" "false")
	  (if frontpage "true" "false") 
	  (if before (format nil ",before:\"~A\"" before) "")
	  (if after (format nil ",after:\"~A\"" after) ""))) 

(defun get-posts ()
  (let ((cached-result (and *background-loader-thread* (cache-get "index-json" "new-not-meta")))) 
    (if cached-result
      (rest (cadar (json:decode-json-from-string cached-result)))
      (lw2-graphql-query (make-posts-list-query)))))

(defun get-posts-json ()
  (lw2-graphql-query-noparse (make-posts-list-query)))

(defun get-post-body (post-id)
  (lw2-graphql-query-timeout-cached (format nil "{PostsSingle(documentId:\"~A\") {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url, htmlBody}}" post-id) "post-body-json" post-id))

(defun get-post-comments (post-id)
  (lw2-graphql-query-timeout-cached (format nil "{CommentsList (terms:{view:\"postCommentsTop\",limit:100,postId:\"~A\"}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}" post-id) "post-comments-json" post-id))

(defun get-recent-comments ()
  (let ((cached-result (and *background-loader-thread* (cache-get "index-json" "recent-comments"))))
    (if cached-result
      (rest (cadar (json:decode-json-from-string cached-result))) 
      (lw2-graphql-query (format nil "{CommentsList (terms:{view:\"postCommentsNew\",limit:20}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}")))))

(defun get-recent-comments-json ()
  (lw2-graphql-query-noparse (format nil "{CommentsList (terms:{view:\"postCommentsNew\",limit:20}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}")))

(defun get-post-title-real (post-id)
  (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {title}}" post-id))) 

(defun cache-post-title (post-id title)
  (cache-put "postid-to-title" post-id title))

(defun get-post-title (post-id)
  (let ((post-title (cache-get "postid-to-title" post-id)))
    (if post-title post-title
      (handler-case
	(let ((data (get-post-title-real post-id)))
	  (assert data)
	  (cache-post-title post-id (cdr (first data))))
	(t () "[Error communicating with LW2 server]"))))) 

(defun get-username-real (user-id)
  (lw2-graphql-query (format nil "{UsersSingle (documentId:\"~A\") {username}}" user-id))) 

(defun get-username (user-id)
  (let ((username (cache-get "userid-to-username" user-id)))
    (if username username
      (handler-case
	(let ((data (get-username-real user-id)))
	  (assert data)
	  (cache-put "userid-to-username" user-id (cdr (first data))))
	(t () "[Error communicating with LW2 server]"))))) 

(defun log-condition (condition)
  (with-open-file (outstream "./logs/error.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format outstream "~%~A: ~S ~A~%" (local-time:format-timestring nil (local-time:now)) condition condition)
    (sb-debug:print-backtrace :stream outstream :from :interrupted-frame :print-frame-source t))) 

(defmacro log-conditions (&body body)
  `(handler-case (progn ,@body)
     (t (c) (log-condition c)))) 

(defun background-loader ()
  (loop
    (log-conditions 
      (let ((posts-json (get-posts-json)))
	(if (and posts-json (ignore-errors (json:decode-json-from-string posts-json)))
	  (cache-put "index-json" "new-not-meta" posts-json))))
    (log-conditions
      (let ((recent-comments-json (get-recent-comments-json)))
	(if (and recent-comments-json (ignore-errors (json:decode-json-from-string recent-comments-json)))
	  (cache-put "index-json" "recent-comments" recent-comments-json)))) 
    (sleep 60))) 

(defun start-background-loader ()
  (assert (not *background-loader-thread*))
  (setf *background-loader-thread* (sb-thread:make-thread #'background-loader))) 

(defun stop-background-loader ()
  (sb-thread:terminate-thread *background-loader-thread*)
  (setf *background-loader-thread* nil)) 

(defun pretty-time (timestring)
  (local-time:format-timestring nil (local-time:parse-timestring timestring)
				:format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone))) 

(defun post-headline-to-html (post)
  (let ((id (cdr (assoc :--id post)))
	(title (cdr (assoc :title post))))
    (if (and id title) (cache-post-title (cdr (assoc :--id post)) (cdr (assoc :title post))))) 
  (format nil "<h1 class=\"listing\"><a href=\"~A\">~A</a></h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A points</div><a class=\"comment-count\" href=\"/post?id=~A#comments\">~A comments</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a>~A</div>"
	  (or (cdr (assoc :url post)) (format nil "/post?id=~A" (url-rewrite:url-encode (cdr (assoc :--id post))))) 
	  (cdr (assoc :title post))
	  (get-username (cdr (assoc :user-id post)))
	  (pretty-time (cdr (assoc :posted-at post))) 
	  (cdr (assoc :base-score post))
	  (url-rewrite:url-encode (cdr (assoc :--id post))) 
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post))
	  (if (cdr (assoc :url post)) (format nil "<div class=\"link-post\">(~A)</div>" (puri:uri-host (puri:parse-uri (cdr (assoc :url post))))) ""))) 

(defun post-body-to-html (post)
  (let ((id (cdr (assoc :--id post)))
	(title (cdr (assoc :title post))))
    (if (and id title) (cache-post-title (cdr (assoc :--id post)) (cdr (assoc :title post))))) 
  (format nil "<div class=\"post\"><h1>~A</h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A points</div><a class=\"comment-count\" href=\"#comments\">~A comments</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a></div><div class=\"post-body\">~A</div></div>"
	  (cdr (assoc :title post))
	  (get-username (cdr (assoc :user-id post)))
	  (pretty-time (cdr (assoc :posted-at post))) 
	  (cdr (assoc :base-score post))
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post)) 
	  (format nil "~A~A"
		  (if (cdr (assoc :url post)) (format nil "<p><a href=\"~A\">Link post</a></p>" (cdr (assoc :url post))) "")
		  (or (cdr (assoc :html-body post)) "")))) 

(defun comment-to-html (comment &key with-post-title)
  (format nil "<div class=\"comment\"><div class=\"comment-meta\"><div>~A</div><a id=\"~A\" href=\"~A\">~A</a><div>~A points</div><a href=\"~A#~A\">LW2 link</a>~A</div><div class=\"comment-body\">~A</div></div>"
	  (get-username (cdr (assoc :user-id comment))) 
	  (cdr (assoc :--id comment)) 
	  (format nil "/post?id=~A#~A" (cdr (assoc :post-id comment)) (cdr (assoc :--id comment))) 
	  (pretty-time (cdr (assoc :posted-at comment)))
	  (cdr (assoc :base-score comment))
	  (cdr (assoc :page-url comment)) 
	  (cdr (assoc :--id comment)) 
	  (if with-post-title
	    (format nil "<div class=\"comment-post-title\">on: <a href=\"/post?id=~A\">~A</a></div>" (cdr (assoc :post-id comment)) (get-post-title (cdr (assoc :post-id comment))))
	    "") 
	  (cdr (assoc :html-body comment)))) 

(defun make-comment-parent-hash (comments)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (c comments)
      (let* ((parent-id (cdr (assoc :parent-comment-id c)))
	     (old (gethash parent-id hash)))
	(setf (gethash parent-id hash) (cons c old))))
    (maphash (lambda (k old)
	       (setf (gethash k hash) (nreverse old)))
	     hash) 
    hash)) 

(defun comment-tree-to-html (comment-hash &optional (target nil))
  (let ((comments (gethash target comment-hash)))
    (if comments 
      (format nil "<ul class=\"comment-thread\">~{<li class=\"comment-item\">~A</li>~}</ul>"
	      (map 'list (lambda (c)
			   (format nil "~A~A"
				   (comment-to-html c)
				   (comment-tree-to-html comment-hash (cdr (assoc :--id c)))))
		   comments))
      ""))) 

(defparameter *html-head*
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"/style.css\">")

(defparameter *nav-items* '(("home" "/" "Home" "Latest frontpage posts")
			    ("featured" "/index?view=featured" "Featured" "Latest featured posts")
			    ("all" "/index?view=new&all=t" "All" "Latest frontpage posts and userpage posts") 
			    ("meta" "/index?view=meta&all=t" "Meta" "Latest meta posts")
			    ("recent-comments" "/recentcomments" "Recent Comments" "Latest comments"))) 

(defun nav-bar-to-html (&optional current-uri)
  (format nil "<div id=\"nav-bar\">~{~A~}</div>"
	  (map 'list (lambda (item)
		       (destructuring-bind (id uri name description) item
			 (if (string= uri current-uri)
			   (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-current\" title=\"~A\">~A</span>" id description name) 
			   (format nil "<span id=\"nav-item-~A\" class=\"nav-item\" title=\"~A\"><a href=\"~A\">~A</a></span>" id description uri name))))
	       *nav-items*))) 

(defparameter *bottom-bar*
"<div id=\"bottom-bar\"><a href=\"#top\">Back to top</a></div>") 

(hunchentoot:define-easy-handler (say-yo :uri "/") ()
				 (setf (hunchentoot:content-type*) "text/html")
				 (let ((posts (get-posts)))
				   (format nil "<html lang=\"en-US\"><head><title>LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~A~{~A~}~A</div></body></html>"
					   *html-head*
					   (nav-bar-to-html (hunchentoot:request-uri*)) 
					   (map 'list #'post-headline-to-html posts)
					   *bottom-bar*)))

(hunchentoot:define-easy-handler (view-index :uri "/index") (view all meta before after)
				 (setf (hunchentoot:content-type*) "text/html")
				 (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :frontpage (not all) :meta (not (not meta)) :before before :after after))))
				   (format nil "<html lang=\"en-US\"><head><title>LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~A~{~A~}~A</div></body></html>"
					   *html-head*
					   (nav-bar-to-html (hunchentoot:request-uri*)) 
					   (map 'list #'post-headline-to-html posts)
					   *bottom-bar*))) 

(hunchentoot:define-easy-handler (view-post :uri "/post") (id)
				 (setf (hunchentoot:content-type*) "text/html")
				 (handler-case 
				   (progn
				     (unless (and id (not (equal id ""))) (error "No post ID.")) 
				     (let ((post (get-post-body id))) 
				       (format nil "<html lang=\"en-US\"><head><title>~A - LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~A~A<div id=\"comments\">~A</div>~A</div></body></html>"
					       (cdr (assoc :title post)) 
					       *html-head*
					       (nav-bar-to-html (hunchentoot:request-uri*)) 
					       (post-body-to-html post) 
					       (let ((comments (get-post-comments id)))
						 (comment-tree-to-html (make-comment-parent-hash comments)))
					       *bottom-bar*))) 
				   (t (condition)
				      (setf (hunchentoot:return-code*) 500)
				      (format nil "<html lang=\"en-US\"><head><title>Error - LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~A<h1>Error</h1><p>~A</p></div></body></html>"
					      *html-head*
					      (nav-bar-to-html (hunchentoot:request-uri*)) 
					      condition)))) 
 
(hunchentoot:define-easy-handler (view-recent-comments :uri "/recentcomments") ()
				 (setf (hunchentoot:content-type*) "text/html")
				 (let ((recent-comments (get-recent-comments)))
				   (format nil "<html lang=\"en-US\"><head><title>Recent comments - LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~A<ul class=\"comment-thread\">~{<li class=\"comment-item\">~A</li>~}</ul>~A</div></body></html>"
					   *html-head*
					   (nav-bar-to-html (hunchentoot:request-uri*)) 
					   (map 'list (lambda (c) (comment-to-html c :with-post-title t)) recent-comments)
					   *bottom-bar*)))

