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
  (setq *db-environment* (lmdb:make-environment *cache-db* :mapsize (expt 2 30))) 
  (lmdb:open-environment *db-environment*)) 

(defmacro with-db ((db db-name) &body body)
  (alexandria:with-gensyms (txn)
			   `(with-mutex (*db-mutex*)
					(let ((,txn (lmdb:make-transaction *db-environment*)))
					  (unwind-protect
					    (progn 
					      (lmdb:begin-transaction ,txn)
					      (let ((,db (lmdb:make-database ,txn ,db-name)))
						(lmdb:with-database (,db)
								    (prog1
								      (progn
									,@body)
								      (lmdb:commit-transaction ,txn)
								      (setf ,txn nil)))))
					    (when ,txn (lmdb:abort-transaction ,txn)))))))

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
      (decode-graphql-json cached-result)
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

(defun lw2-search-query (query)
  (let ((req-stream (drakma:http-request "https://z0gr6exqhd-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20vanilla%20JavaScript%203.24.5%3Breact-instantsearch%204.1.3%3BJS%20Helper%202.23.0&x-algolia-application-id=Z0GR6EXQHD&x-algolia-api-key=0b1d20b957917dbb5e1c2f3ad1d04ee2"
					 :method :post :additional-headers '(("Origin" . "https://www.greaterwrong.com") ("Referer" . "https://www.greaterwrong.com/"))
					 :content (format nil "{\"requests\":[{\"indexName\":\"test_posts\",\"params\":\"query=~A&hitsPerPage=20&page=0\"}]}" (url-rewrite:url-encode query))
					 :want-stream t)))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (cdr (assoc :hits (first (cdr (assoc :results (json:decode-json req-stream))))))))

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
	(when (and posts-json (ignore-errors (json:decode-json-from-string posts-json)))
	  (cache-put "index-json" "new-not-meta" posts-json)
	  (with-db (db "postid-to-title")
		   (dolist (post (decode-graphql-json posts-json))
		     (lmdb-put-string db (cdr (assoc :--id post)) (cdr (assoc :title post))))))))
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
  (labels ((scan-for-urls (text-node)
			  (let ((text (plump:text text-node)))
			    (multiple-value-bind (url-start url-end) (ppcre:scan "(https?://[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+|[-a-zA-Z0-9.]+\\.(com|edu|gov|mil|net|org|biz|info|name|museum|us|ca|uk))(\\:[0-9]+){0,1}(/[-a-zA-Z0-9.,;?'\\\\+&%$#=~_/]*)?" text)
			      (when url-start
				(let* ((url-raw (subseq text url-start url-end))
				       (url (if (mismatch "http" url-raw :end2 4) (concatenate 'string "http://" url-raw) url-raw)) 
				       (temp-root (plump:make-root))
				       (new-a (plump:make-element (plump:make-root) "a"))
				       (new-text (if (= url-end (length text)) nil (plump:make-text-node temp-root (subseq text url-end))))) 
				  (setf (plump:text text-node) (subseq text 0 url-start)
					(plump:attribute new-a "href") url)
				  (plump:make-text-node new-a url-raw)
				  (if new-text (plump:insert-after text-node new-text))
				  (plump:insert-after text-node new-a)))))) 
	   (contents-to-html (contents)
			     (format nil "<div class=\"contents\"><div class=\"contents-head\">Contents</div><ul>~{~A~}</ul></div>"
				     (map 'list (lambda (x) (destructuring-bind (elem-level text id) x
							      (declare (ignore elem-level)) 
							      (format nil "<li><a href=\"#~A\">~A</a></li>"
								      id text)))
					  contents))))
    (let ((root (plump:parse in-html))
	  (contents nil)
	  (section-count 0))
      (plump:traverse root (lambda (node)
			     (typecase node
			       (plump:text-node 
				 (when (and (plump:text-node-p node) (or (typep (plump:parent node) 'plump:root) (every (lambda (x) (string/= (plump:tag-name (plump:parent node)) x)) '("a" "style"))))
				   (scan-for-urls node)))
			       (plump:element 
				 (when (string= (plump:tag-name node) "a")
				   (let ((href (plump:attribute node "href")))
				     (when href
				       (multiple-value-bind (story-id comment-id) (match-lw2-link href)
					 (when story-id
					   (setf (plump:attribute node "href") (generate-post-link story-id comment-id)))))))
				 (when (or (string= (plump:tag-name node) "p") (string= (plump:tag-name node) "blockquote"))
				   (unless (plump:has-child-nodes node)
				     (plump:remove-child node)))
				 (when (ppcre:scan "^h[1-6]$" (plump:tag-name node))
				   (incf section-count) 
				   (unless (plump:attribute node "id") (setf (plump:attribute node "id") (format nil "section-~A" section-count))) 
				   (push (list (parse-integer (subseq (plump:tag-name node) 1))
					       (plump:text node)
					       (plump:attribute node "id"))
					 contents)))))) 
      (concatenate 'string (if (> section-count 3) (contents-to-html (nreverse contents)) "") 
		   (plump:serialize root nil)))))

(defun pretty-time (timestring &key format)
  (local-time:format-timestring nil (local-time:parse-timestring timestring)
				:format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))) 

(defun post-headline-to-html (post)
  (format nil "<h1 class=\"listing\"><a href=\"~A\">~A</a></h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A point~:P</div><a class=\"comment-count\" href=\"/post?id=~A#comments\">~A comment~:P</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a>~A</div>"
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
  (format nil "<div class=\"post\"><h1>~A</h1><div class=\"post-meta\"><div class=\"author\">~A</div><div class=\"date\">~A</div><div class=\"karma\">~A point~:P</div><a class=\"comment-count\" href=\"#comments\">~A comment~:P</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a></div><div class=\"post-body\">~A</div></div>"
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
  (format nil "<div class=\"comment\"><div class=\"comment-meta\"><div>~A</div><a href=\"~A\">~A</a><div>~A point~:P</div><a href=\"~A#~A\">LW2 link</a>~A</div><div class=\"comment-body\">~A</div></div>"
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

(defun comment-tree-to-html (comment-hash &optional (target nil) (level 0))
  (let ((comments (gethash target comment-hash)))
    (if comments 
      (format nil "<ul class=\"comment-thread\">~{~A~}</ul>"
	      (map 'list (lambda (c)
			   (let ((c-id (cdr (assoc :--id c)))) 
			   (format nil "<li id=\"~A\" class=\"comment-item\">~A~A~A</li>"
				   c-id
				   (comment-to-html c)
				   (if (and (= level 4) (gethash c-id comment-hash))
				     (format nil "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\">Expand this thread</label>"
					     c-id)
				     "") 
				   (comment-tree-to-html comment-hash c-id (1+ level)))))
		   comments))
      ""))) 

(defparameter *html-head*
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"/style.css\">")

(defun search-bar-to-html ()
  (declare (special *current-search-query*))
  (let ((query (and (boundp '*current-search-query*) (hunchentoot:escape-for-html *current-search-query*))))
    (format nil "<form action=\"/search\" class=\"nav-inner\"><input name=\"q\" type=\"search\" ~@[value=\"~A\"~] autocomplete=\"off\"><button>Search</button></form>" query))) 

(defparameter *primary-nav* '(("home" "/" "Home" :description "Latest frontpage posts")
			      ("featured" "/index?view=featured" "Featured" :description "Latest featured posts")
			      ("all" "/index?view=new&all=t" "All" :description "Latest frontpage posts and userpage posts") 
			      ("meta" "/index?view=meta&all=t" "Meta" :description "Latest meta posts")
			      ("recent-comments" "/recentcomments" "Recent Comments" :description "Latest comments"))) 

(defparameter *secondary-nav* `(("about" "/about" "About")
				("rss" "/feed" "RSS")
				("search" "/search" "Search" :html ,#'search-bar-to-html))) 

(defun nav-bar-to-html (&optional current-uri)
  (let ((primary-bar "primary-bar")
	(secondary-bar "secondary-bar")
	active-bar) 
    (labels ((nav-bar-inner (bar-id items) 
			    (format nil "~{~A~}"
				    (maplist (lambda (items)
					       (let ((item (first items))) 
						 (destructuring-bind (id uri name &key description html) item
						   (if (string= uri current-uri)
						     (progn (setf active-bar bar-id) 
							    (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-current\" ~@[title=\"~A\"~]>~:[<span class=\"nav-inner\">~A</span>~;~:*~A~]</span>"
								    id description (and html (funcall html)) name)) 
						     (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-inactive~:[~; nav-item-last-before-current~]\" ~@[title=\"~A\"~]>~:[<a href=\"~A\" class=\"nav-inner\">~A</a>~;~:*~A~]</span>"
							     id (string= (nth 1 (cadr items)) current-uri) description (and html (funcall html)) uri name)))))
					 items)))
	     (nav-bar-outer (id class html)
			    (format nil "<div id=\"~A\" class=\"nav-bar ~A\">~A</div>" id class html)))
      (let ((primary-html (nav-bar-inner primary-bar *primary-nav*))
	    (secondary-html (nav-bar-inner secondary-bar *secondary-nav*)))
	(if (eq active-bar secondary-bar) 
	  (format nil "~A~A" (nav-bar-outer primary-bar "inactive-bar" primary-html) (nav-bar-outer secondary-bar "active-bar" secondary-html))
	  (format nil "~A~A" (nav-bar-outer secondary-bar "inactive-bar" secondary-html) (nav-bar-outer primary-bar "active-bar" primary-html))))))) 

(defparameter *bottom-bar*
"<div id=\"bottom-bar\" class=\"nav-bar\"><a class=\"nav-item nav-current nav-inner\" href=\"#top\">Back to top</a></div>") 

(defun begin-html (out-stream &key title description current-uri content-class)
  (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head><title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~A</head><body><div id=\"content\"~@[ class=\"~A\"~]>~A"
	  title description
	  *html-head* content-class
	  (nav-bar-to-html (or current-uri (hunchentoot:request-uri*))))) 

(defun end-html (out-stream)
  (format out-stream "~A</div></body></html>" *bottom-bar*)) 

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defmacro emit-page ((out-stream &key title description current-uri content-class (return-code 200)) &body body)
  `(log-conditions
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
	   (hunchentoot:return-code*) ,return-code) 
     (let ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8))) 
       (begin-html ,out-stream :title ,title :description ,description :current-uri ,current-uri :content-class ,content-class)
       ,@body
       (end-html ,out-stream)))) 

(defmacro with-error-page (&body body)
  `(handler-case
     (progn ,@body)
     (t (condition)
	(log-condition condition)
	(emit-page (out-stream :title "Error" :return-code 500) 
		   (format out-stream "<h1>Error</h1><p>~A</p>"
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

(hunchentoot:define-easy-handler (view-search :uri "/search") (q)
				 (with-error-page
				   (let ((posts (lw2-search-query q))
					 (*current-search-query* q)) 
				     (declare (special *current-search-query*)) 
				     (emit-page (out-stream :title "Search" :current-uri "/search" :content-class "search-results-page")
						(map-output out-stream #'post-headline-to-html posts))))) 

(hunchentoot:define-easy-handler (view-about :uri "/about") ()
				 (with-error-page
				   (emit-page (out-stream :title "About")
					      (with-outputs (out-stream) "<h1>About</h1><p>About page goes here.</p>")))) 
