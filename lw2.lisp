(defpackage lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams))

(in-package #:lw2-viewer) 

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))
;(defvar *lw2-stream* nil) 
(defvar *username-cache* (make-hash-table :test 'equal)) 

(defparameter *cache-db* "./cache/") 
(defvar *db-mutex* (sb-thread:make-mutex :name "lmdb")) 
(defvar *db-environment*) 

(when (not (boundp '*db-environment*))
  (setq *db-environment* (lmdb:make-environment *cache-db*)) 
  (lmdb:open-environment *db-environment*)) 

(defmacro with-db ((db db-name) &body body)
  (alexandria:with-gensyms (txn)
			   `(with-mutex (*db-mutex*)
					(let ((,txn (lmdb:make-transaction *db-environment*)))
					  (lmdb:begin-transaction ,txn)
					  (let ((,db (lmdb:make-database ,txn ,db-name)))
					    (lmdb:with-database (,db)
								(prog1
								  (progn
								    ,@body)
								  (lmdb:commit-transaction ,txn))))))))

(defun cache-put (db-name key value)
  (with-db (db db-name) 
	   (if (lmdb:put db (string-to-octets key :external-format :utf-8)
			 (string-to-octets value :external-format :utf-8))
	     value
	     nil))) 

(defun cache-get (db-name key)
  (with-db (db db-name) 
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

(defun lw2-graphql-query-timeout-cached (query cache-db cache-key)
  (let* ((cached-result (cache-get cache-db cache-key)) 
	 (timeout nil) ;(if cached-result 2 nil) 
	 (thread (ensure-cache-update-thread query cache-db cache-key))) 
    (decode-graphql-json
      (handler-case
	(sb-thread:join-thread thread :timeout timeout)
	(t () (or cached-result
		  (error "Failed to load ~A ~A and no cached version available." cache-db cache-key)))))))

(declaim (inline make-posts-list-query)) 
(defun make-posts-list-query (&key (view "new") (limit 20) (meta nil) (frontpage t) (before nil) (after nil) (with-body nil))
  (declare (type string view)
	   (type (integer 1) limit)
	   (type boolean meta)
	   (type (or string null) before after))
  (format nil "{PostsList (terms:{view:\"~A\",limit:~A,meta:~A~A~A~A}) {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url~A}}"
	  view
	  limit
	  (if meta "true" "false")
	  (if frontpage ",frontpage:true" "") 
	  (if before (format nil ",before:\"~A\"" before) "")
	  (if after (format nil ",after:\"~A\"" after) "")
	  (if with-body ", htmlBody" ""))) 

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

(defun make-simple-cache (cache-db)
  (lambda (key value) (cache-put cache-db key value))) 

(defun make-simple-get (cache-db cache-fn get-real-fn)
  (lambda (key) 
    (let ((val (cache-get cache-db key)))
      (if val val
	(handler-case
	  (let ((data (funcall get-real-fn key)))
	    (assert data)
	    (funcall cache-fn key (cdr (first data))))
	  (t () "[Error communicating with LW2 server]")))))) 

(defmacro simple-cacheable ((base-name cache-db key) &body body)
  (let ((get-real (intern (format nil "~:@(get-~A-real~)" base-name)))
	(cache (intern (format nil "~:@(cache-~A~)" base-name)))
	(get (intern (format nil "~:@(get-~A~)" base-name))))
    `(setf (fdefinition (quote ,get-real)) (lambda (,key) ,@body)
	   (fdefinition (quote ,cache)) (make-simple-cache ,cache-db)
	   (fdefinition (quote ,get)) (make-simple-get ,cache-db (fdefinition (quote ,cache)) (fdefinition (quote ,get-real)))))) 

(simple-cacheable ("post-title" "postid-to-title" post-id)
  (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {title}}" post-id))) 

(simple-cacheable ("username" "userid-to-displayname" user-id)
  (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {displayName}}" user-id))) 

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

(defun match-lw2-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(^https?://(www.)?lesserwrong.com|^)/posts/([^/]+)/[^/]*(/([^/]+))?$" link)
    (when match?
      (values (elt strings 2) (elt strings 4))))) 
 
(defun generate-post-link (story-id &optional comment-id absolute-uri)
  (format nil "~Apost?id=~A~@[#~A~]" (if absolute-uri *site-uri* "/") story-id comment-id)) 

(defun clean-html (in-html)
  (let ((root (plump:parse in-html)))
    (dolist (n (plump:get-elements-by-tag-name root "a"))
      (let ((href (plump:attribute n "href")))
	(when href
	  (multiple-value-bind (story-id comment-id) (match-lw2-link href)
	    (when story-id
	      (setf (plump:attribute n "href") (generate-post-link story-id comment-id)))))))
    (plump:serialize root nil)))

(defun pretty-time (timestring &key format)
  (local-time:format-timestring nil (local-time:parse-timestring timestring)
				:format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))) 

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

(defun posts-to-rss (posts out-stream)
  (xml-emitter:with-rss2 (out-stream :encoding "UTF-8")
			 (xml-emitter:rss-channel-header "LessWrong 2 viewer" *site-uri*
							 :description "LessWrong 2 viewer") 
			 (dolist (post posts)
			   (xml-emitter:rss-item
			     (cdr (assoc :title post))
			     :link (generate-post-link (cdr (assoc :--id post)) nil t)
			     :author (get-username (cdr (assoc :user-id post)))
			     :pubDate (pretty-time (cdr (assoc :posted-at post)) :format local-time:+rfc-1123-format+)
			     :description (clean-html (or (cdr (assoc :html-body post)) "")))))) 

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
		  (clean-html (or (cdr (assoc :html-body post)) ""))))) 

(defun comment-to-html (comment &key with-post-title)
  (format nil "<div class=\"comment\"><div class=\"comment-meta\"><div>~A</div><a href=\"~A\">~A</a><div>~A points</div><a href=\"~A#~A\">LW2 link</a>~A</div><div class=\"comment-body\">~A</div></div>"
	  (get-username (cdr (assoc :user-id comment))) 
	  (format nil "/post?id=~A#~A" (cdr (assoc :post-id comment)) (cdr (assoc :--id comment))) 
	  (pretty-time (cdr (assoc :posted-at comment)))
	  (cdr (assoc :base-score comment))
	  (cdr (assoc :page-url comment)) 
	  (cdr (assoc :--id comment)) 
	  (if with-post-title
	    (format nil "<div class=\"comment-post-title\">on: <a href=\"/post?id=~A\">~A</a></div>" (cdr (assoc :post-id comment)) (get-post-title (cdr (assoc :post-id comment))))
	    "") 
	  (clean-html (cdr (assoc :html-body comment))))) 

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
      (format nil "<ul class=\"comment-thread\">~{~A~}</ul>"
	      (map 'list (lambda (c)
			   (format nil "<li id=\"~A\" class=\"comment-item\">~A~A</li>"
				   (cdr (assoc :--id c)) 
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

(defun begin-html (out-stream &key title description)
  (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head><title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~A</head><body><div id=\"content\">~A"
	  title description
	  *html-head* (nav-bar-to-html (hunchentoot:request-uri*)))) 

(defun end-html (out-stream)
  (format out-stream "</div></body></html>")) 

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defmacro emit-page ((out-stream &key title description (return-code 200)) &body body)
  `(log-conditions
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
	   (hunchentoot:return-code*) ,return-code) 
     (let ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8))) 
       (begin-html ,out-stream :title ,title :description ,description)
       ,@body
       (end-html ,out-stream)))) 

(defmacro with-error-page (&body body)
  `(handler-case
     (progn ,@body)
     (t (condition)
	(log-condition condition)
	(emit-page (out-stream :title "Error" :return-code 500) 
		   (format nil "<h1>Error</h1><p>~A</p>"
			   condition))))) 

(hunchentoot:define-easy-handler (say-yo :uri "/") ()
				 (with-error-page 
				   (let ((posts (get-posts)))
				     (emit-page (out-stream :description "A faster way to browse LessWrong 2.0") 
						(map-output out-stream #'post-headline-to-html posts)))))

(hunchentoot:define-easy-handler (view-index :uri "/index") (view all meta before after)
				 (with-error-page 
				   (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :frontpage (not all) :meta (not (not meta)) :before before :after after))))
				     (emit-page (out-stream :description "A faster way to browse LessWrong 2.0") 
						(map-output out-stream #'post-headline-to-html posts))))) 

(hunchentoot:define-easy-handler (view-post :uri "/post") (id)
				 (with-error-page
				   (unless (and id (not (equal id ""))) (error "No post ID.")) 
				   (let ((post (get-post-body id))) 
				     (emit-page (out-stream :title (cdr (assoc :title post))) 
						(with-outputs (out-stream) (post-body-to-html post)) 
						(format out-stream "<div id=\"comments\">~A</div>"
							(let ((comments (get-post-comments id)))
							  (comment-tree-to-html (make-comment-parent-hash comments)))))))) 

(hunchentoot:define-easy-handler (view-feed :uri "/feed") ()
				 (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
				 (let ((posts (lw2-graphql-query (make-posts-list-query :with-body t)))
				       (out-stream (hunchentoot:send-headers)))
				   (posts-to-rss posts (make-flexi-stream out-stream :external-format :utf-8)))) 

(hunchentoot:define-easy-handler (view-post-lw2-link :uri (lambda (r) (declare (ignore r)) (match-lw2-link (hunchentoot:request-uri*)))) ()
				 (multiple-value-bind (post-id comment-id) (match-lw2-link (hunchentoot:request-uri*))
				   (setf (hunchentoot:return-code*) 303
					 (hunchentoot:header-out "Location") (generate-post-link post-id comment-id)))) 

(hunchentoot:define-easy-handler (view-recent-comments :uri "/recentcomments") ()
				 (with-error-page
				   (let ((recent-comments (get-recent-comments)))
				     (emit-page (out-stream :title "Recent comments" :description "A faster way to browse LessWrong 2.0") 
						(with-outputs (out-stream) "<ul class=\"comment-thread\">") 
						(map-output out-stream (lambda (c) (format nil "<li class=\"comment-item\">~A</li>" (comment-to-html c :with-post-title t))) recent-comments)
						(with-outputs (out-stream) "</ul>")))))

