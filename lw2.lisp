(use-package :flexi-streams) 

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))
;(defvar *lw2-stream* nil) 
(defvar *username-cache* (make-hash-table :test 'equal)) 

(defparameter *cache-db* "./cache/") 

(defun cache-put (db key value)
  (let ((env (lmdb:make-environment *cache-db*))) 
    (lmdb:with-environment (env)
			   (let ((txn (lmdb:make-transaction env)))
			     (lmdb:begin-transaction txn)
			     (let ((db (lmdb:make-database txn db)))
			       (lmdb:with-database (db)
						   (prog1 (if (lmdb:put db (string-to-octets key :external-format :utf-8)
									(string-to-octets value :external-format :utf-8))
							    value
							    nil) 
						     (lmdb:commit-transaction txn)))))))) 

(defun cache-get (db key)
  (let ((env (lmdb:make-environment *cache-db*))) 
    (lmdb:with-environment (env)
			   (let ((txn (lmdb:make-transaction env)))
			     (lmdb:begin-transaction txn)
			     (let ((db (lmdb:make-database txn db)))
			       (lmdb:with-database (db)
						   (let ((result (lmdb:get db (string-to-octets key :external-format :utf-8)))) 
						     (prog1 (if result
							      (octets-to-string result :external-format :utf-8)
							      nil)
						       (lmdb:commit-transaction txn))))))))) 

(defvar *background-loader-thread* nil) 

(defun lw2-graphql-query (query)
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

(defun get-posts ()
  (let ((cached-result (and *background-loader-thread* (cache-get "index-json" "new-not-meta")))) 
    (if cached-result
      (rest (cadar (json:decode-json-from-string cached-result)))
      (lw2-graphql-query "{PostsList (terms:{view:\"new\",limit:20,meta:false}) {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url}}"))))

(defun get-posts-json ()
  (lw2-graphql-query-noparse "{PostsList (terms:{view:\"new\",limit:20,meta:false}) {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url}}"))

(defun get-post-body (post-id)
  (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {title, _id, userId, postedAt, baseScore, commentCount, pageUrl, url, htmlBody}}" post-id)))

(defun get-post-comments (post-id)
  (lw2-graphql-query (format nil "{CommentsList (terms:{view:\"postCommentsTop\",limit:100,postId:\"~A\"}) {_id, userId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}" post-id)))

(defun get-username-real (user-id)
  (lw2-graphql-query (format nil "{UsersSingle (documentId:\"~A\") {username}}" user-id))) 

(defun get-username (user-id)
  (let ((username (cache-get "userid-to-username" user-id)))
    (if username username
      (let ((data (get-username-real user-id)))
	(cache-put "userid-to-username" user-id (cdr (first data))))))) 

(defun background-loader ()
  (loop
    (let ((posts-json (get-posts-json)))
      (if posts-json
	(cache-put "index-json" "new-not-meta" posts-json)))
    (sleep 30))) 

(defun start-background-loader ()
  (assert (not *background-loader-thread*))
  (setf *background-loader-thread* (sb-thread:make-thread #'background-loader))) 

(defun stop-background-loader ()
  (sb-thread:terminate-thread *background-loader-thread*)
  (setf *background-loader-thread* nil)) 

(defun post-headline-to-html (post)
  (format nil "<h1 class=\"listing\"><a href=\"~A\">~A</a></h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A points</div><a class=\"comment-count\" href=\"/post?id=~A#comments\">~A comments</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a>~A</div>"
	  (or (cdr (assoc :url post)) (format nil "/post?id=~A" (url-rewrite:url-encode (cdr (assoc :--id post))))) 
	  (cdr (assoc :title post))
	  (get-username (cdr (assoc :user-id post)))
	  (cdr (assoc :posted-at post)) 
	  (cdr (assoc :base-score post))
	  (url-rewrite:url-encode (cdr (assoc :--id post))) 
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post))
	  (if (cdr (assoc :url post)) (format nil "<div class=\"link-post\">(~A)</div>" (puri:uri-host (puri:parse-uri (cdr (assoc :url post))))) ""))) 

(defun post-body-to-html (post)
  (format nil "<div class=\"post\"><h1>~A</h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A points</div><a class=\"comment-count\" href=\"#comments\">~A comments</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a></div><div class=\"post-body\">~A</div></div>"
	  (cdr (assoc :title post))
	  (get-username (cdr (assoc :user-id post)))
	  (cdr (assoc :posted-at post)) 
	  (cdr (assoc :base-score post))
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post)) 
	  (format nil "~A~A"
		  (if (cdr (assoc :url post)) (format nil "<p><a href=\"~A\">Link post</a></p>" (cdr (assoc :url post))) "")
		  (or (cdr (assoc :html-body post)) "")))) 

(defun comment-to-html (comment)
  (format nil "<div class=\"comment\"><div class=\"comment-meta\"><div>~A</div><div>~A</div><div>~A points</div><a href=\"~A#~A\">LW2 link</a></em></div><div class=\"comment-body\">~A</div>"
	  (get-username (cdr (assoc :user-id comment))) 
	  (cdr (assoc :posted-at comment))
	  (cdr (assoc :base-score comment))
	  (cdr (assoc :page-url comment)) 
	  (cdr (assoc :--id comment)) 
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
      (format nil "<ul>~{<li>~A</li>~}</ul>"
	      (map 'list (lambda (c)
			   (format nil "~A~A"
				   (comment-to-html c)
				   (comment-tree-to-html comment-hash (cdr (assoc :--id c)))))
		   comments))
      ""))) 

(defparameter *html-head*
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"/style.css\">")

(hunchentoot:define-easy-handler (say-yo :uri "/") ()
				 (setf (hunchentoot:content-type*) "text/html")
				 (let ((posts (get-posts)))
				   (format nil "<html lang=\"en-US\"><head><title>LessWrong 2 viewer</title>~A</head><body><div id=\"content\">~{~A~}<a href=\"#top\">Back to top</a></div></body></html>"
					   *html-head*
					   (map 'list #'post-headline-to-html posts))))

(hunchentoot:define-easy-handler (view-post :uri "/post") (id)
				 (setf (hunchentoot:content-type*) "text/html")
				 (let ((post (get-post-body id))) 
				   (format nil "<html lang=\"en-US\"><head><title>~A - LessWrong 2 viewer</title>~A</head><body><div id=\"content\"><a href=\"/\">Home</a>~A<hr><a name=\"comments\"></a><div id=\"comments\">~A</div><a href=\"#top\">Back to top</a></div></body></html>"
					   (cdr (assoc :title post)) 
					   *html-head*
					   (post-body-to-html post) 
					   (let ((comments (get-post-comments id)))
					     (comment-tree-to-html (make-comment-parent-hash comments)))))) 
