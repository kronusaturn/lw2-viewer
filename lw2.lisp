(defpackage lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:lw2-viewer.config #:lw2.login))

(in-package #:lw2-viewer) 

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defvar *current-auth-token*) 

(defparameter *cache-db* "./cache/") 
(defvar *db-mutex* (sb-thread:make-mutex :name "lmdb")) 
(defvar *db-environment*) 

(when (not (boundp '*db-environment*))
  (setq *db-environment* (lmdb:make-environment *cache-db* :max-databases 1024 :mapsize (expt 2 34)))
  (lmdb:open-environment *db-environment*)) 

(defmacro with-db ((db db-name) &body body)
  (alexandria:with-gensyms (txn)
			   `(with-mutex (*db-mutex*)
					(let ((,txn (lmdb:make-transaction *db-environment* :flags 0))
					      (,db (lmdb:make-database ,db-name)))
					  (unwind-protect
					    (progn 
					      (lmdb:begin-transaction ,txn)
					      (let ((lmdb:*transaction* ,txn))
						(unwind-protect
						  (progn 
						    (lmdb:open-database ,db :create t) 
						    (prog1
						      (progn
							,@body)
						      (lmdb:commit-transaction ,txn)
						      (setf ,txn nil)))
						  (lmdb:close-database ,db)))) 
					    (when ,txn (lmdb:abort-transaction ,txn)))))))

(defun lmdb-clear-db (db)
  (lmdb:do-pairs (db key value)
		 (lmdb:del db key nil))) 

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
    (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
			 :cookie-jar *cookie-jar* :want-stream t)
    (declare (ignore status-code headers final-uri reuse-stream))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (rest (cadar (json:decode-json req-stream))))) 

(defun lw2-graphql-query-noparse (query)
    (octets-to-string (drakma:http-request *graphql-uri* :parameters (list (cons "query" query))
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

(declaim (inline make-posts-list-query)) 
(defun make-posts-list-query (&key (view "new") (limit 20) (meta nil) (frontpage t) (before nil) (after nil) (with-body nil))
  (declare (type string view)
	   (type (integer 1) limit)
	   (type boolean meta)
	   (type (or string null) before after))
  (format nil "{PostsList (terms:{view:\"~A\",limit:~A,meta:~A~A~A~A}) {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url~A}}"
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

(defun get-post-body (post-id &key (revalidate t))
  (lw2-graphql-query-timeout-cached (format nil "{PostsSingle(documentId:\"~A\") {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url, htmlBody}}" post-id) "post-body-json" post-id :revalidate revalidate))

(defun get-post-comments (post-id)
  (lw2-graphql-query-timeout-cached (format nil "{CommentsList (terms:{view:\"postCommentsTop\",limit:10000,postId:\"~A\"}) {_id, userId, postId, postedAt, parentCommentId, baseScore, pageUrl, htmlBody}}" post-id) "post-comments-json" post-id))

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
					 :content (json:encode-json-alist-to-string `(("requests" . ,(loop for index in '("test_posts" "test_comments")
													   collect `(("indexName" . ,index)
														     ("params" . ,(format nil "query=~A&hitsPerPage=20&page=0"
																	  (url-rewrite:url-encode query)))))))) 
					 :want-stream t)))
    (setf (flexi-stream-external-format req-stream) :utf-8)
    (values-list (loop for r in (cdr (assoc :results (json:decode-json req-stream)))
		       collect (cdr (assoc :hits r))))))

(defun make-simple-cache (cache-db)
  (lambda (key value) (cache-put cache-db key value))) 

(defun make-simple-get (cache-db cache-fn get-real-fn)
  (lambda (key) 
    (let ((val (cache-get cache-db key)))
      (if val val
	(handler-case
	  (let ((data (funcall get-real-fn key)))
	    (assert data)
	    (funcall cache-fn key data))
	  (t () "[Error communicating with LW2 server]")))))) 

(defmacro simple-cacheable ((base-name cache-db key) &body body)
  (let ((get-real (intern (format nil "~:@(get-~A-real~)" base-name)))
	(cache (intern (format nil "~:@(cache-~A~)" base-name)))
	(get (intern (format nil "~:@(get-~A~)" base-name))))
    `(setf (fdefinition (quote ,get-real)) (lambda (,key) ,@body)
	   (fdefinition (quote ,cache)) (make-simple-cache ,cache-db)
	   (fdefinition (quote ,get)) (make-simple-get ,cache-db (fdefinition (quote ,cache)) (fdefinition (quote ,get-real)))))) 

(simple-cacheable ("post-title" "postid-to-title" post-id)
  (cdr (first (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {title}}" post-id))))) 

(simple-cacheable ("post-slug" "postid-to-slug" post-id)
  (cdr (first (lw2-graphql-query (format nil "{PostsSingle(documentId:\"~A\") {slug}}" post-id)))))

(simple-cacheable ("username" "userid-to-displayname" user-id)
  (cdr (first (lw2-graphql-query (format nil "{UsersSingle(documentId:\"~A\") {displayName}}" user-id))))) 

(defun preload-username-cache ()
  (let ((user-list (lw2-graphql-query "{UsersList(terms:{}) {_id, displayName}}")))
    (loop for user in user-list
	  do (cache-username (cdr (assoc :--id user)) (cdr (assoc :display-name user)))))) 

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
  (with-mutex (*db-mutex*) 
    (sb-thread:terminate-thread *background-loader-thread*))
  (setf *background-loader-thread* nil)) 

(defun match-lw1-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(?:^https?://(?:www.)?less(?:er)?wrong.com|^)(?:/r/discussion)?(/lw/.*)" link)
    (when match?
      (values (elt strings 0))))) 

(simple-cacheable ("lw1-link" "lw1-link" link)
  (let ((out (nth-value 3 (drakma:http-request (concatenate 'string "https://www.lesserwrong.com" link) :method :head))))
    (format nil "~A~@[#~A~]" (puri:uri-path out) (puri:uri-fragment out)))) 

(defun convert-lw1-link (link)
  (alexandria:if-let (canonical-link (match-lw1-link link))
		     (get-lw1-link canonical-link))) 

(defun match-lw2-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(^https?://(www.)?lesserwrong.com|^)/posts/([^/]+)/([^/]*)(/$|/([^/#]+))?(#|$)" link)
    (when match?
      (values (elt strings 2) (elt strings 5) (elt strings 3))))) 

(labels
  ((gen-internal (post-id slug comment-id &optional absolute-uri)
		 (format nil "~Aposts/~A/~A~@[#~A~]" (if absolute-uri *site-uri* "/") post-id (or slug "-") comment-id))) 

  (defun convert-lw2-link (link)
    (multiple-value-bind (post-id comment-id slug) (match-lw2-link link)
      (when post-id 
	(gen-internal post-id slug comment-id)))) 

  (defun generate-post-link (story &optional comment-id absolute-uri) 
    (typecase story
      (string 
	(gen-internal story (get-post-slug story) comment-id absolute-uri))
      (cons
	(gen-internal (cdr (assoc :--id story)) (or (cdr (assoc :slug story)) (get-post-slug story)) comment-id absolute-uri))))) 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun city-hash-128-vector (data)
    (apply #'concatenate
	   (cons 'vector (map 'list #'bit-smasher:int->octets
			      (multiple-value-list
				(city-hash:city-hash-128 data)))))) 

  (defun hash-printable-object (object)
    (city-hash-128-vector (string-to-octets (prin1-to-string object) :external-format :utf-8))))

(defmacro define-lmdb-memoized (&whole whole name lambda &body body)
  (let ((db-name (concatenate 'string (string-downcase (symbol-name name)) "-memo"))
	(version-octets (string-to-octets "version" :external-format :utf-8))
	(now-hash (hash-printable-object whole)))
    (alexandria:once-only (db-name version-octets now-hash)
			  `(progn
			     (unless (equalp ,now-hash (with-db (db ,db-name) (lmdb:get db ,version-octets)))
			       (with-db (db ,db-name)
					(lmdb-clear-db db)
					(lmdb:put db ,version-octets ,now-hash)))
			     (defun ,name (&rest args)
			       (labels ((real-fn ,lambda ,@body))
				 (let* ((hash (hash-printable-object args))
					(cached-value (with-db (db ,db-name) (lmdb:get db hash))))
				   (if cached-value
				     (octets-to-string cached-value :external-format :utf-8)
				     (let ((new-value (apply #'real-fn args)))
				       (with-db (db ,db-name) (lmdb:put db hash (string-to-octets new-value :external-format :utf-8)))
				       new-value))))))))) 

(defvar *memory-intensive-mutex* (sb-thread:make-mutex :name "memory-intensive-mutex")) 

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun grab-from-rts (url)
  (let* ((root (plump:parse (drakma:http-request url)))
	 (post-body (plump:get-element-by-id root "wikitext")))
    (loop for cls in '("div.nav_menu" "div.imgonly" "div.bottom_nav") do
	  (loop for e across (clss:select cls post-body)
		do (plump:remove-child e))) 
    (plump:remove-child (elt (clss:select "h1" post-body) 0))
    (plump:remove-child (elt (clss:select "p" post-body) 0))
    (with-open-file (stream (merge-pathnames "./rts-content/" (subseq (puri:uri-path (puri:parse-uri url)) 1)) :direction :output :if-does-not-exist :create :external-format :utf-8) 
		 (plump:serialize post-body stream))))

(defun rts-to-html (file)
  (concatenate 'string
	       "<style>"
	       (file-get-contents "./rts-content/rts.css")
	       "</style>"
	       (file-get-contents (merge-pathnames "./rts-content/" file)))) 

(defparameter *html-overrides* (make-hash-table :test 'equal))
(loop for (id file) in '(("XTXWPQSEgoMkAupKt" "An-Intuitive-Explanation-Of-Bayess-Theorem") 
			 ("afmj8TKAqH6F2QMfZ" "A-Technical-Explanation-Of-Technical-Explanation")
			 ("7ZqGiPHTpiDMwqMN2" "The-Twelve-Virtues-Of-Rationality"))
      do (let ((file* file)) (setf (gethash id *html-overrides*) (lambda () (rts-to-html file*)))))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defparameter *text-clean-regexps*
    (let ((data (destructuring-bind (* ((* (* inner))))
		  (with-open-file (stream "text-clean-regexps.js") (parse-js:parse-js stream))
		  inner)))
      (loop for input in data
	    collecting (destructuring-bind (* ((* regex flags) (* replacement))) input
			 (list regex flags (ppcre:regex-replace-all "\\$(\\d)" replacement "\\\\\\1"))))))) 

(defun clean-text (text)
  (macrolet ((inner () `(progn ,@(loop for (regex flags replacement) in *text-clean-regexps*
				       collecting `(setf text (ppcre:regex-replace-all
								(ppcre:create-scanner ,regex
										      ,@(loop for (flag sym) in '((#\i :case-insensitive-mode)
														  (#\m :multi-line-mode)
														  (#\s :single-line-mode)
														  (#\x :extended-mode))
											      when (find flag flags)
											      append (list sym t)))
								text ,replacement))))))
    (inner)))

(define-lmdb-memoized clean-html (in-html &key with-toc post-id)
  (with-recursive-lock (*memory-intensive-mutex*) ; this is actually thread-safe, but running it concurrently risks running out of memory
    (labels ((tag-is (node &rest args)
		     (declare (type plump:node node)
			      (dynamic-extent args))
		     (let ((tag (plump:tag-name node)))
		       (some (lambda (x) (string= tag x))
			     args))) 
	     (only-child-is (node &rest args)
			    (declare (type plump:node node)
				     (dynamic-extent args)) 
			    (and (= 1 (length (plump:children node)))
				 (let ((child (plump:first-child node))) 
				   (and 
				     (typep child 'plump:element)
				     (apply #'tag-is (cons child args)))))) 
	     (text-node-is-not (node &rest args)
			       (declare (type plump:node node) 
					(dynamic-extent args)) 
			       (or
				 (typep (plump:parent node) 'plump:root)
				 (every (lambda (x) (string/= (plump:tag-name (plump:parent node)) x)) args))) 
	     (scan-for-urls (text-node)
			    (declare (type plump:text-node text-node)) 
			    (let ((text (plump:text text-node)))
			      (multiple-value-bind (url-start url-end) (ppcre:scan "(https?://[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+|[-a-zA-Z0-9.]+\\.(com|edu|gov|mil|net|org|biz|info|name|museum|us|ca|uk))(\\:[0-9]+){0,1}(/[-a-zA-Z0-9.,;?'\\\\+&%$#=~_/]*)?" text)
				(when url-start
				  (let* ((url-raw (subseq text url-start url-end))
					 (url (if (mismatch "http" url-raw :end2 4) (concatenate 'string "http://" url-raw) url-raw)) 
					 (family (plump:family text-node)) 
					 (other-children (prog1
							   (subseq family (1+ (plump:child-position text-node)))
							   (setf (fill-pointer family) (1+ (plump:child-position text-node))))) 
					 (new-a (plump:make-element (plump:parent text-node) "a"))
					 (new-text (unless (= url-end (length text)) (plump:make-text-node (plump:parent text-node) (subseq text url-end))))) 
				    (setf (plump:text text-node) (subseq text 0 url-start)
					  (plump:attribute new-a "href") (or (convert-lw2-link url) (convert-lw1-link url) url))
				    (plump:make-text-node new-a (clean-text url-raw))
				    (when new-text
				      (scan-for-urls new-text)
				      (setf (plump:text new-text) (clean-text (plump:text new-text))))
				    (loop for item across other-children
					  do (plump:append-child (plump:parent text-node) item)))))))
	     (contents-to-html (contents min-header-level)
			       (declare (type cons contents)) 
			       (format nil "<div class=\"contents\"><div class=\"contents-head\">Contents</div><ul>~{~A~}</ul></div>"
				       (map 'list (lambda (x) (destructuring-bind (elem-level text id) x
								(format nil "<li class=\"toc-item-~A\"><a href=\"#~A\">~A</a></li>"
									(- elem-level (- min-header-level 1)) id text)))
					    contents)))
	     (style-hash-to-html (style-hash)
				 (declare (type hash-table style-hash))
				 (let ((style-list (alexandria:hash-table-keys style-hash)))
				   (if style-list
				     (format nil "<style>~{~A~}</style>" style-list)
				     ""))))
      (handler-bind
	(((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
	(alexandria:if-let
	  (override (gethash post-id *html-overrides*))
	  (funcall override) 
	  (let ((root (plump:parse (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) in-html)))
		(contents nil)
		(section-count 0)
		(min-header-level 6) 
		(aggressive-deformat nil) 
		(style-hash (make-hash-table :test 'equal)))
	    (loop while (and (= 1 (length (plump:children root))) (typep (plump:first-child root) 'plump:element) (tag-is (plump:first-child root) "div"))
		  do (setf (plump:children root) (plump:children (plump:first-child root)))) 
	    (plump:traverse root (lambda (node)
				   (typecase node
				     (plump:text-node 
				       (when (text-node-is-not node "a" "style" "pre")
					 (scan-for-urls node))
				       (when (text-node-is-not node "style" "pre" "code")
					 (setf (plump:text node) (clean-text (plump:text node)))))
				     (plump:element
				       (alexandria:when-let (style (plump:attribute node "style"))
					 (when (or aggressive-deformat (search "font-family" style))
					   (setf aggressive-deformat t) 
					   (plump:remove-attribute node "style"))) 
				       (when (and aggressive-deformat (tag-is node "div"))
					 (setf (plump:tag-name node) "p")) 
				       (when (tag-is node "a")
					 (let ((href (plump:attribute node "href")))
					   (when href
					     (let ((new-link (or (convert-lw2-link href) (convert-lw1-link href))))
					       (when new-link
						 (setf (plump:attribute node "href") new-link)))))
					 (when (only-child-is node "u")
					   (setf (plump:children node) (plump:children (plump:first-child node)))))
				       (when (tag-is node "p" "blockquote" "div")
					 (when (every (lambda (c) (cl-unicode:has-binary-property c "White_Space")) (plump:text node)) 
					   (plump:remove-child node)))
				       (when (tag-is node "u")
					 (when (only-child-is node "a")
					   (plump:replace-child node (plump:first-child node)))) 
				       (when (and with-toc (ppcre:scan "^h[1-6]$" (plump:tag-name node)))
					 (incf section-count) 
					 (unless (plump:attribute node "id") (setf (plump:attribute node "id") (format nil "section-~A" section-count))) 
					 (let ((header-level (parse-integer (subseq (plump:tag-name node) 1))))
					   (setf min-header-level (min min-header-level header-level)) 
					   (push (list header-level
						       (plump:text node)
						       (plump:attribute node "id"))
						 contents)))
				       (when (tag-is node "style")
					 (setf (gethash (plump:text node) style-hash) t)
					 (plump:remove-child node)))))) 
	    (concatenate 'string (if (> section-count 3) (contents-to-html (nreverse contents) min-header-level) "") 
			 (style-hash-to-html style-hash) 
			 (plump:serialize root nil))))))))

(defun logged-in-userid (&optional is-userid)
  (let ((current-userid (and *current-auth-token* (cache-get "auth-token-to-userid" *current-auth-token*))))
    (if is-userid
      (string= current-userid is-userid)
      current-userid))) 
(defun logged-in-username ()
  (and *current-auth-token* (cache-get "auth-token-to-username" *current-auth-token*))) 

(defun pretty-time (timestring &key format)
  (local-time:format-timestring nil (local-time:parse-timestring timestring)
				:format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))) 

(defun post-headline-to-html (post)
  (format nil "<h1 class=\"listing\"><a href=\"~A\">~A</a></h1><div class=\"post-meta\"><div class=\"author\">~A</div> <div class=\"date\">~A</div><div class=\"karma\">~A point~:P</div><a class=\"comment-count\" href=\"~A#comments\">~A comment~:P</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a>~A</div>"
	  (or (cdr (assoc :url post)) (generate-post-link post)) 
	  (clean-text (cdr (assoc :title post)))
	  (get-username (cdr (assoc :user-id post)))
	  (pretty-time (cdr (assoc :posted-at post))) 
	  (cdr (assoc :base-score post))
	  (generate-post-link post) 
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post))
	  (if (cdr (assoc :url post)) (format nil "<div class=\"link-post\">(~A)</div>" (puri:uri-host (puri:parse-uri (cdr (assoc :url post))))) ""))) 

(defun posts-to-rss (posts out-stream)
  (with-recursive-lock (*memory-intensive-mutex*) 
    (xml-emitter:with-rss2 (out-stream :encoding "UTF-8")
      (xml-emitter:rss-channel-header "LessWrong 2 viewer" *site-uri*
				      :description "LessWrong 2 viewer") 
      (dolist (post posts)
	(xml-emitter:rss-item
	  (clean-text (cdr (assoc :title post)))
	  :link (generate-post-link post nil t)
	  :author (get-username (cdr (assoc :user-id post)))
	  :pubDate (pretty-time (cdr (assoc :posted-at post)) :format local-time:+rfc-1123-format+)
	  :description (clean-html (or (cdr (assoc :html-body (get-post-body (cdr (assoc :--id post)) :revalidate nil))) "") :post-id (cdr (assoc :--id post)))))))) 

(defun post-body-to-html (post)
  (format nil "<div class=\"post\"><h1>~A</h1><div class=\"post-meta\"><div class=\"author\">~A</div> <div class=\"date\">~A</div><div class=\"karma\">~A point~:P</div><a class=\"comment-count\" href=\"#comments\">~A comment~:P</a><a class=\"lw2-link\" href=\"~A\">LW2 link</a><a href=\"#bottom-bar\"></a></div><div class=\"post-body\">~A</div></div>"
	  (clean-text (cdr (assoc :title post)))
	  (get-username (cdr (assoc :user-id post)))
	  (pretty-time (cdr (assoc :posted-at post))) 
	  (cdr (assoc :base-score post))
	  (or (cdr (assoc :comment-count post)) 0) 
	  (cdr (assoc :page-url post)) 
	  (format nil "~A~A"
		  (if (cdr (assoc :url post)) (format nil "<p><a href=\"~A\">Link post</a></p>" (cdr (assoc :url post))) "")
		  (clean-html (or (cdr (assoc :html-body post)) "") :with-toc t :post-id (cdr (assoc :--id post)))))) 

(defun comment-to-html (comment &key with-post-title)
  (format nil "<div class=\"comment\"><div class=\"comment-meta\"><div class=\"author\">~A</div> <a class=\"date\" title=\"~:*~A at ~*~A~2:*\" href=\"~A\">~A</a><div class=\"karma\">~A point~:P</div><a class=\"lw2-link\" href=\"~A#~A\">LW2 link</a>~A</div><div class=\"comment-body\"~@[ data-markdown-source=\"~A\"~]>~A</div></div>"
	  (get-username (cdr (assoc :user-id comment))) 
	  (generate-post-link (cdr (assoc :post-id comment)) (cdr (assoc :--id comment))) 
	  (pretty-time (cdr (assoc :posted-at comment)))
	  (cdr (assoc :base-score comment))
	  (cdr (assoc :page-url comment)) 
	  (cdr (assoc :--id comment)) 
	  (if with-post-title
	    (format nil "<div class=\"comment-post-title\">on: <a href=\"~A\">~A</a></div>" (generate-post-link (cdr (assoc :post-id comment))) (clean-text (get-post-title (cdr (assoc :post-id comment)))))
	    (format nil "~@[<a class=\"comment-parent-link\" href=\"#~A\">Parent</a>~]" (cdr (assoc :parent-comment-id comment)))) 
	  (if (logged-in-userid (cdr (assoc :user-id comment)))
	    (plump:encode-entities
	      (or (cache-get "comment-markdown-source" (cdr (assoc :--id comment))) 
		  (cdr (assoc :html-body comment)))))
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
    (labels
      ((count-children (parent)
	(let ((children (gethash (cdr (assoc :--id parent)) hash)))
	  (+ (length children) (apply #'+ (map 'list #'count-children children))))) 
       (add-child-counts (comment-list) 
	(loop for c in comment-list
	      as id = (cdr (assoc :--id c)) 
	      do (setf (gethash id hash) (add-child-counts (gethash id hash))) 
	      collecting (cons (cons :child-count (count-children c)) c))))
      (setf (gethash nil hash) (add-child-counts (gethash nil hash)))) 
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
				   (if (and (= level 10) (gethash c-id comment-hash))
				     (format nil "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\" data-child-count=\"~A comment~:P\">Expand this thread</label>"
					     c-id
					     (cdr (assoc :child-count c)))
				     "") 
				   (comment-tree-to-html comment-hash c-id (1+ level)))))
		   comments))
      ""))) 

(defparameter *html-head*
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"//fonts.greaterwrong.com/?fonts=Charter,Concourse,a_Avante\">")

(defun generate-versioned-link (file)
  (format nil "~A?v=~A" file (sb-posix:stat-mtime (sb-posix:stat (format nil "www~A" file))))) 

(defun search-bar-to-html ()
  (declare (special *current-search-query*))
  (let ((query (and (boundp '*current-search-query*) (hunchentoot:escape-for-html *current-search-query*))))
    (format nil "<form action=\"/search\" class=\"nav-inner\"><input name=\"q\" type=\"search\" ~@[value=\"~A\"~] autocomplete=\"off\"><button>Search</button></form>" query)))  

(defparameter *primary-nav* '(("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
			      ("featured" "/index?view=featured" "Featured" :description "Latest featured posts" :accesskey "f")
			      ("all" "/index?view=new&all=t" "All" :description "Latest frontpage posts and userpage posts" :accesskey "a") 
			      ("meta" "/index?view=meta&all=t" "Meta" :description "Latest meta posts" :accesskey "m")
			      ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c"))) 

(defparameter *secondary-nav* `(("archive" "/archive" "Archive")
				("search" "/search" "Search" :html ,#'search-bar-to-html)
				("login" "/login" "Log In"))) 

(defun nav-bar-to-html (&optional current-uri)
  (let ((primary-bar "primary-bar")
	(secondary-bar "secondary-bar")
	active-bar) 
    (labels ((nav-bar-inner (bar-id items) 
			    (format nil "~{~A~}"
				    (maplist (lambda (items)
					       (let ((item (first items))) 
						 (destructuring-bind (id uri name &key description html accesskey) item
						   (if (string= uri current-uri)
						     (progn (setf active-bar bar-id) 
							    (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-current\" ~@[title=\"~A\"~]>~:[<span class=\"nav-inner\">~A</span>~;~:*~A~]</span>"
								    id description (and html (funcall html)) name)) 
						     (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-inactive~:[~; nav-item-last-before-current~]\" ~@[title=\"~A\"~]>~:[<a href=\"~A\" class=\"nav-inner\" ~@[accesskey=\"~A\"~]>~A</a>~;~:*~A~]</span>"
							     id (string= (nth 1 (cadr items)) current-uri) (if accesskey (format nil "~A [~A]" description accesskey) description) (and html (funcall html)) uri accesskey name)))))
					 items)))
	     (nav-bar-outer (id class html)
			    (format nil "<div id=\"~A\" class=\"nav-bar ~A\">~A</div>" id class html)))
      (let ((primary-html (nav-bar-inner primary-bar *primary-nav*))
	    (secondary-html (nav-bar-inner secondary-bar *secondary-nav*)))
	(if (eq active-bar secondary-bar) 
	  (format nil "~A~A" (nav-bar-outer primary-bar "inactive-bar" primary-html) (nav-bar-outer secondary-bar "active-bar" secondary-html))
	  (format nil "~A~A" (nav-bar-outer secondary-bar "inactive-bar" secondary-html) (nav-bar-outer primary-bar "active-bar" primary-html))))))) 

(defun user-nav-bar (&optional current-uri)
  (let* ((username (logged-in-username)))
    (let ((*secondary-nav* `(("archive" "/archive" "Archive")
			     ("search" "/search" "Search" :html ,#'search-bar-to-html)
			     ,(if username
				`("login" "/login" ,username)
				`("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In")))))
      (nav-bar-to-html current-uri)))) 

(defparameter *bottom-bar*
"<div id=\"bottom-bar\" class=\"nav-bar\"><a class=\"nav-item nav-current nav-inner\" href=\"#top\">Back to top</a></div>") 

(defun make-csrf-token (session-token &optional (nonce (ironclad:make-random-salt)))
  (if (typep session-token 'string) (setf session-token (base64:base64-string-to-usb8-array session-token)))
  (let ((csrf-token (concatenate '(vector (unsigned-byte 8)) nonce (ironclad:digest-sequence :sha256 (concatenate '(vector (unsigned-byte 8)) nonce session-token)))))
    (values (base64:usb8-array-to-base64-string csrf-token) csrf-token))) 

(defun check-csrf-token (session-token csrf-token)
  (let* ((session-token (base64:base64-string-to-usb8-array session-token))
	 (csrf-token (base64:base64-string-to-usb8-array csrf-token))
	 (correct-token (nth-value 1 (make-csrf-token session-token (subseq csrf-token 0 16)))))
    (assert (ironclad:constant-time-equal csrf-token correct-token) nil "CSRF check failed.")
    t)) 

(defun begin-html (out-stream &key title description current-uri content-class)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
	 (csrf-token (and session-token (make-csrf-token session-token)))) 
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head><title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~A<link rel=\"stylesheet\" href=\"~A\"><link rel=\"shortcut icon\" href=\"~A\"><script src=\"~A\" async></script><script src=\"~A\" async></script>~@[<script>var csrfToken=\"~A\"</script>~]</head><body><div id=\"content\"~@[ class=\"~A\"~]>~A"
	    title description
	    *html-head* (generate-versioned-link "/style.css") (generate-versioned-link "/favicon.ico") (generate-versioned-link "/script.js") (generate-versioned-link "/guiedit.js")
	    csrf-token
	    content-class
	    (user-nav-bar (or current-uri (hunchentoot:request-uri*)))))
  (force-output out-stream)) 

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
  `(ignore-errors
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
	   (hunchentoot:return-code*) ,return-code) 
     (let ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8))) 
       (begin-html ,out-stream :title ,title :description ,description :current-uri ,current-uri :content-class ,content-class)
       ,@body
       (end-html ,out-stream)))) 

(defmacro with-error-page (&body body)
  `(let ((*current-auth-token* (hunchentoot:cookie-in "lw2-auth-token")))
     (handler-case
       (log-conditions 
	 (progn ,@body))
       (serious-condition (condition)
			  (emit-page (out-stream :title "Error" :return-code 500) 
				     (format out-stream "<h1>Error</h1><p>~A</p>"
					     condition)))))) 

(defun view-posts-index (posts)
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
		     ("rss" 
		      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
		      (let ((out-stream (hunchentoot:send-headers)))
			(posts-to-rss posts (make-flexi-stream out-stream :external-format :utf-8))))
		     (t
		       (emit-page (out-stream :description "A faster way to browse LessWrong 2.0") 
				  (format out-stream "<a class=\"rss\" rel=\"alternate\" type=\"application/rss+xml\" href=\"~A?~@[~A&~]format=rss\">RSS</a>"
					  (hunchentoot:script-name*) (hunchentoot:query-string*)) 
				  (map-output out-stream #'post-headline-to-html posts))))) 

(hunchentoot:define-easy-handler (view-root :uri "/") ()
				 (with-error-page (view-posts-index (get-posts))))

(hunchentoot:define-easy-handler (view-index :uri "/index") (view all meta before after)
				 (with-error-page
				   (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :frontpage (not all) :meta (not (not meta)) :before before :after after))))
				     (view-posts-index posts)))) 

(hunchentoot:define-easy-handler (view-post :uri "/post") (id)
				 (with-error-page
				   (unless (and id (not (equal id ""))) (error "No post ID.")) 
				   (setf (hunchentoot:return-code*) 301
					 (hunchentoot:header-out "Location") (generate-post-link id)))) 

(hunchentoot:define-easy-handler (view-post-lw1-link :uri (lambda (r) (match-lw1-link (hunchentoot:request-uri r)))) ()
				 (with-error-page
				   (let ((location (convert-lw1-link (hunchentoot:request-uri*)))) 
				     (setf (hunchentoot:return-code*) 301
					   (hunchentoot:header-out "Location") location)))) 

(hunchentoot:define-easy-handler (view-feed :uri "/feed") (view all meta before after)
				 (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
				 (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :frontpage (not all) :meta (not (not meta)) :before before :after after)))
				       (out-stream (hunchentoot:send-headers)))
				   (posts-to-rss posts (make-flexi-stream out-stream :external-format :utf-8)))) 

(hunchentoot:define-easy-handler (view-post-lw2-link :uri (lambda (r) (declare (ignore r)) (match-lw2-link (hunchentoot:request-uri*)))) ((csrf-token :request-type :post) (text :request-type :post) (parent-comment-id :request-type :post) (edit-comment-id :request-type :post))
				 (with-error-page
				   (cond
				     (text
				       (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
				       (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token"))
					     (post-id (match-lw2-link (hunchentoot:request-uri*)))) 
					 (assert (and lw2-auth-token (not (string= text ""))))
					 (let* ((comment-data `(("body" . ,text) ,(if (not edit-comment-id) `("postId" . ,post-id)) ,(if parent-comment-id `("parentCommentId" . ,parent-comment-id)) ("content" . ("blocks" . nil)))) 
						(new-comment-id
						  (if edit-comment-id
						    (prog1 edit-comment-id
						      (do-lw2-comment-edit lw2-auth-token edit-comment-id comment-data))
						    (do-lw2-comment lw2-auth-token comment-data))))
					   (cache-put "comment-markdown-source" new-comment-id text)
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (concatenate 'string (hunchentoot:request-uri*) "#" new-comment-id)))))
				     (t 
				       (multiple-value-bind (post-id comment-id) (match-lw2-link (hunchentoot:request-uri*))
					 (if comment-id 
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (generate-post-link post-id comment-id))
					   (let ((post (get-post-body post-id))) 
					     (emit-page (out-stream :title (clean-text (cdr (assoc :title post)))) 
							(with-outputs (out-stream) (post-body-to-html post)) 
							(force-output out-stream) 
							(format out-stream "<div id=\"comments\">~A</div>"
								(let ((comments (get-post-comments post-id)))
								  (comment-tree-to-html (make-comment-parent-hash comments)))))))))))) 

(hunchentoot:define-easy-handler (view-recent-comments :uri "/recentcomments") ()
				 (with-error-page
				   (let ((recent-comments (get-recent-comments)))
				     (emit-page (out-stream :title "Recent comments" :description "A faster way to browse LessWrong 2.0") 
						(with-outputs (out-stream) "<ul class=\"comment-thread\">") 
						(map-output out-stream (lambda (c) (format nil "<li class=\"comment-item\">~A</li>" (comment-to-html c :with-post-title t))) recent-comments)
						(with-outputs (out-stream) "</ul>")))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body (markdown:parse (cdr (assoc :body item)))) item)) 

(hunchentoot:define-easy-handler (view-search :uri "/search") (q)
				 (with-error-page
				   (let ((*current-search-query* q)) 
				     (declare (special *current-search-query*)) 
				     (multiple-value-bind (posts comments) (lw2-search-query q)
				       (emit-page (out-stream :title "Search" :current-uri "/search" :content-class "search-results-page")
						  (map-output out-stream #'post-headline-to-html posts)
						  (with-outputs (out-stream) "<ul class=\"comment-thread\">") 
						  (map-output out-stream (lambda (c) (format nil "<li class=\"comment-item\">~A</li>" (comment-to-html (search-result-markdown-to-html c) :with-post-title t))) comments)
						  (with-outputs (out-stream) "</ul>")))))) 

(defun output-form (out-stream method action id class csrf-token fields button-label)
  (format out-stream "<form method=\"~A\" action=\"~A\" id=\"~A\" class=\"~A\"><div>" method action id class)
  (loop for (id label type) in fields
	do (format out-stream "<div><label for=\"~A\">~A:</label><input type=\"~A\" name=\"~@*~A\"></div>" id label type))
  (format out-stream "<div><input type=\"hidden\" name=\"csrf-token\" value=\"~A\"><input type=\"submit\" value=\"~A\"></div></div></form>" csrf-token button-label)) 

(hunchentoot:define-easy-handler (view-login :uri "/login") (return cookie-check (csrf-token :request-type :post) (login-username :request-type :post) (login-password :request-type :post)
								    (signup-username :request-type :post) (signup-email :request-type :post) (signup-password :request-type :post) (signup-password2 :request-type :post))
				 (with-error-page
				   (labels
				     ((emit-login-page (&key error-message)
					(let ((csrf-token (make-csrf-token (hunchentoot:cookie-in "session-token"))))
					  (emit-page (out-stream :title "Log in" :current-uri "/login" :content-class "login-page")
						     (when error-message
						       (format out-stream "<div class=\"error-box\">~A</div>" error-message)) 
						     (with-outputs (out-stream) "<div class=\"login-container\"><div id=\"login-form-container\"><h1>Log in</h1>")
						     (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "login-form" "aligned-form" csrf-token
								  '(("login-username" "Username" "text")
								    ("login-password" "Password" "password"))
								  "Log in")
						     (with-outputs (out-stream) "</div><div id=\"create-account-form-container\"><h1>Create account</h1>")
						     (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "signup-form" "aligned-form" csrf-token
								  '(("signup-username" "Username" "text")
								    ("signup-email" "Email" "text")
								    ("signup-password" "Password" "password")
								    ("signup-password2" "Confirm password" "password"))
								  "Create account")
						     (with-outputs (out-stream) "</div></div>"))))) 
				     (cond
				       ((not (or cookie-check (hunchentoot:cookie-in "session-token")))
					(hunchentoot:set-cookie "session-token" :value (base64:usb8-array-to-base64-string (ironclad:make-random-salt)))
					(setf (hunchentoot:return-code*) 303
					      (hunchentoot:header-out "Location") (format nil "/login?~@[return=~A&~]cookie-check=y" (if return (url-rewrite:url-encode return))))) 
				       (cookie-check
					 (if (hunchentoot:cookie-in "session-token")
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))))
					   (emit-page (out-stream :title "Log in" :current-uri "/login")
						      (format out-stream "<h1>Enable cookies</h1><p>Please enable cookies in your browser and <a href=\"/login~@[?return=~A~]\">try again</a>.</p>" (if return (url-rewrite:url-encode return)))))) 
				       (login-username
					 (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
					 (cond
					   ((or (string= login-username "") (string= login-password "")) (emit-login-page :error-message "Please enter a username and password")) 
					   (t (multiple-value-bind (user-id auth-token error-message) (do-lw2-login "username" login-username login-password) 
						(cond
						  (auth-token
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :max-age (- (expt 2 31) 1)) 
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token login-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[^/]" return)) return "/")))
						  (t
						    (emit-login-page :error-message error-message))))))) 
				       (signup-username
					 (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
					 (cond
					   ((not (every (lambda (x) (not (string= x ""))) (list signup-username signup-email signup-password signup-password2)))
					    (emit-login-page :error-message "Please fill in all fields"))
					   ((not (string= signup-password signup-password2))
					    (emit-login-page :error-message "Passwords do not match"))
					   (t (multiple-value-bind (user-id auth-token error-message) (do-lw2-create-user signup-username signup-email signup-password)
						(cond
						  (error-message (emit-login-page :error-message error-message))
						  (t
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :max-age (- (expt 2 31) 1))
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token signup-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[~/]" return)) return "/"))))))))
				       (t
					 (emit-login-page)))))) 

(defparameter *earliest-post* (local-time:parse-timestring "2005-01-01")) 

(hunchentoot:define-easy-handler (view-archive :uri (lambda (r) (ppcre:scan "^/archive(/|$)" (hunchentoot:request-uri r)))) () 
				 (with-error-page
				   (destructuring-bind (year month day) (map 'list (lambda (x) (if x (parse-integer x)))
									     (nth-value 1 (ppcre:scan-to-strings "^/archive(?:/(\\d{4})|/?$)(?:/(\\d{1,2})|/?$)(?:/(\\d{1,2})|/?$)"
														 (hunchentoot:request-uri*)))) 
				     (labels ((link-if-not (stream linkp url-elements class text)
						       (declare (dynamic-extent linkp url-elements text)) 
						       (if (not linkp)
							 (format stream "<a href=\"/~{~A~^/~}\" class=\"~A\">~A</a>" url-elements class text)
							 (format stream "<span class=\"~A\">~A</span>" class text)))) 
				       (local-time:with-decoded-timestamp (:day current-day :month current-month :year current-year) (local-time:now)
				         (local-time:with-decoded-timestamp (:day earliest-day :month earliest-month :year earliest-year) *earliest-post* 
					   (let ((posts (lw2-graphql-query (format nil "{PostsList (terms:{view:\"~A\",limit:~A~@[,after:\"~A\"~]~@[,before:\"~A\"~]}) {title, _id, slug, userId, postedAt, baseScore, commentCount, pageUrl, url}}"
										   "best" 50
										   (if year (format nil "~A-~A-~A" (or year earliest-year) (or month 1) (or day 1)))
										   (if year (format nil "~A-~A-~A" (or year current-year) (or month 12) (or day (local-time:days-in-month (or month 12) (or year current-year))))))))) 
					     (emit-page (out-stream :title "Archive" :current-uri "/archive" :content-class "archive-page")
							(with-outputs (out-stream) "<div class=\"archive-nav\"><div class=\"archive-nav-years\">")
							(link-if-not out-stream (not (or year month day)) '("archive") "archive-nav-item-year" "All") 
							(loop for y from earliest-year to current-year
							      do (link-if-not out-stream (eq y year) (list "archive" y) "archive-nav-item-year" y))
							(format out-stream "</div>")
							(when year
							  (format out-stream "<div class=\"archive-nav-months\">")
							  (link-if-not out-stream (not month) (list "archive" year) "archive-nav-item-month" "All") 
							  (loop for m from (if (= (or year current-year) earliest-year) earliest-month 1) to (if (= (or year current-year) current-year) current-month 12)
								do (link-if-not out-stream (eq m month) (list "archive" (or year current-year) m) "archive-nav-item-month" (elt local-time:+short-month-names+ m)))
							  (format out-stream "</div>"))
							(when month
							  (format out-stream "<div class=\"archive-nav-days\">")
							  (link-if-not out-stream (not day) (list "archive" year month) "archive-nav-item-day" "All")
							  (loop for d from (if (and (= (or year current-year) earliest-year) (= (or month current-month) earliest-month)) earliest-day 1)
								to (if (and (= (or year current-year) current-year) (= (or month current-month) current-month)) current-day (local-time:days-in-month (or month current-month) (or year current-year)))
								do (link-if-not out-stream (eq d day) (list "archive" (or year current-year) (or month current-month) d) "archive-nav-item-day" d))
							  (format out-stream "</div>")) 
							(format out-stream "</div>") 
							(map-output out-stream #'post-headline-to-html posts))))))))) 

(hunchentoot:define-easy-handler (view-css :uri "/style.css") (v)
				 (when v (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1)))) 
				 (hunchentoot:handle-static-file "www/style.css" "text/css")) 

(defmacro define-versioned-resource (uri content-type)
  `(hunchentoot:define-easy-handler (,(alexandria:symbolicate "versioned-resource-" uri) :uri ,uri) (v)
				    (when v (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1)))) 
				    (hunchentoot:handle-static-file ,(concatenate 'string "www" uri) ,content-type))) 

(define-versioned-resource "/style.css" "text/css") 
(define-versioned-resource "/script.js" "text/javascript") 
(define-versioned-resource "/guiedit.js" "text/javascript") 
(define-versioned-resource "/favicon.ico" "image/x-icon") 
(define-versioned-resource "/fa-regular-400.ttf" "application/x-font-ttf; charset=binary") 
(define-versioned-resource "/fa-solid-900.ttf" "application/x-font-ttf; charset=binary") 
