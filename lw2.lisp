(uiop:define-package #:lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:djula
	#:lw2-viewer.config #:lw2.utils #:lw2.lmdb #:lw2.backend #:lw2.links #:lw2.clean-html #:lw2.login #:lw2.context #:lw2.sites #:lw2.components #:lw2.html-reader #:lw2.fonts
	#:lw2.graphql
	#:lw2.conditions
	#:lw2.routes
	#:lw2.schema-type #:lw2.schema-types
	#:lw2.interface-utils
	#:lw2.user-context
	#:lw2.web-push
	#:lw2.data-viewers.post
	#:lw2.data-viewers.comment)
  (:import-from #:alexandria #:ensure-list #:when-let #:if-let #:alist-hash-table)
  (:import-from #:collectors #:with-collector)
  (:import-from #:ppcre #:regex-replace-all)
  (:unintern
    #:define-regex-handler #:*fonts-stylesheet-uri* #:generate-fonts-link
    #:user-nav-bar #:*primary-nav* #:*secondary-nav* #:*nav-bars*
    #:begin-html #:end-html
    #:*fonts-stylesheet-uris* #:*fonts-redirect-data* #:*fonts-redirect-lock* #:*fonts-redirect-thread*
    #:postprocess-conversation-title
    #:map-output
    #:*earliest-post*)
  (:recycle #:lw2-viewer #:lw2.backend))

(in-package #:lw2-viewer) 

(named-readtables:in-readtable html-reader)

(add-template-directory (asdf:system-relative-pathname "lw2-viewer" "templates/"))

(define-cache-database 'backend-lw2-legacy
    "auth-token-to-userid" "auth-token-to-username" "comment-markdown-source" "post-markdown-source" "user-ignore-list")

(defvar *read-only-mode* nil)
(defvar *read-only-default-message* "Due to a system outage, you cannot log in or post at this time.")

(defparameter *default-prefs* (alist :items-per-page 20 :default-sort "new"))
(defvar *current-prefs* nil)

(defun get-post-sequences (post-id)
  (when-let (sequence-ids (get-post-sequence-ids post-id))
    (with-collector (col)
      (dolist (sequence-id sequence-ids)
	(let* ((sequence (get-sequence sequence-id))
	       (posts (sequence-post-ids sequence)))
	  (multiple-value-bind (prev next)
	      (loop for prev = nil then (car current)
		 for current on posts
		 when (string= (car current) post-id)
		 return (values prev (second current)))
	    (when (or prev next)
	      (col (list sequence
			 (and prev (get-sequence-post sequence prev))
			 (and next (get-sequence-post sequence next))))))))
      (col))))

(defun rectify-post-relations (post-relations)
  (remove-if-not (lambda (pr) (cdr (assoc :--id (cdr (first pr))))) post-relations))

(defun post-nav-links (post post-sequences)
  <nav class="post-nav-links">
    (schema-bind (:post post (target-post-relations source-post-relations) :context :body)
      (let ((target-post-relations (rectify-post-relations target-post-relations))
	    (source-post-relations (rectify-post-relations source-post-relations)))
	(when (or target-post-relations source-post-relations)
	  <div class="related-posts">
	  (dolist (relations `((,source-post-relations "Parent question")
			       (,target-post-relations "Sub-question")))
	    (destructuring-bind (related-posts relation-type) relations
	      (when related-posts
		<div class="related-post-group">
		  <div class="related-post-type">("~A~A" relation-type (if (second related-posts) "s" ""))</div>
		  (dolist (post-relation related-posts)
		    (post-headline-to-html (or (cdr (assoc :source-post post-relation))
					       (cdr (assoc :target-post post-relation)))))
		</div>)))
	  </div>)))
    (loop for (sequence prev next) in post-sequences do
      (progn 
	<div class="post-nav-item sequence">
	  <a class="post-nav sequence-title" href=("/s/~A" (cdr (assoc :--id sequence)))>
	    <span class="post-nav-label">Part of the sequence:</span>
	    <span class="post-nav-title">(safe (clean-text-to-html (cdr (assoc :title sequence))))</span>
	  </a>
	  (labels ((post-nav-link (direction post)
		     <a class=("post-nav ~A" (string-downcase direction)) href=(generate-post-link post)>
		       <span class="post-nav-label">(case direction (:prev "Previous: ") (:next "Next: "))</span>
		       <span class="post-nav-title">(safe (clean-text-to-html (cdr (assoc :title post))))</span>
		     </a>))
	    (when prev (post-nav-link :prev prev))
	    (when next (post-nav-link :next next)))
      </div>))
  </nav>)

(defun rectify-conversation (conversation)
  (alist-bind ((title (or null string)))
	      conversation
    (if (or (null title) (string= title ""))
        (acons :title "[Untitled conversation]" conversation)
        conversation)))

(defun conversation-message-to-html (out-stream message)
  (alist-bind ((user-id string)
               (created-at string)
               (highlight-new boolean)
               (conversation list)
               (content list)
	       (contents list)
               (html-body (or string null)))
	      message
    (let ((conversation (rectify-conversation conversation)))
      (multiple-value-bind (pretty-time js-time) (pretty-time created-at)
	(format out-stream "<div class=\"comment private-message~A\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <span class=\"date\" data-js-date=\"~A\">~A</span><div class=\"comment-post-title\">Private message in: <a href=\"/conversation?id=~A\">~A</a></div></div><div class=\"body-text comment-body\">"
		(if highlight-new " comment-item-highlight" "")
		(encode-entities (get-user-slug user-id))
		(encode-entities (get-username user-id))
		js-time
		pretty-time
		(encode-entities (cdr (assoc :--id conversation)))
		(encode-entities (cdr (assoc :title conversation)))))
      (labels ((ws (html-body) (let ((*memoized-output-stream* out-stream)) (clean-html* html-body))))
	(cond
	  (contents (ws (cdr (assoc :html contents))))
	  (html-body (ws html-body))
	  (t (format out-stream "~{<p>~A</p>~}" (loop for block in (cdr (assoc :blocks content)) collect (encode-entities (cdr (assoc :text block))))))))
      (format out-stream "</div></div>"))))

(defun conversation-index-to-html (out-stream conversation)
  (alist-bind ((conversation-id string :--id)
               (title (or null string))
               (created-at (or null string))
               (participants list)
               (messages-total fixnum))
    (rectify-conversation conversation)
    (multiple-value-bind (pretty-time js-time) (if created-at (pretty-time created-at) (values "[Error]" 0))
      (format out-stream "<h1 class=\"listing\"><a href=\"/conversation?id=~A\">~A</a></h1><div class=\"post-meta\"><div class=\"conversation-participants\"><ul>~:{<li><a href=\"/users/~A\">~A</a></li>~}</ul></div><div class=\"messages-count\">~A</div><div class=\"date\" data-js-date=\"~A\">~A</div></div>"
              (encode-entities conversation-id)
              (encode-entities title)
              (loop for p in participants
                    collect (list (encode-entities (cdr (assoc :slug p))) (encode-entities (cdr (assoc :display-name p)))))
              (pretty-number messages-total "message")
              js-time
              pretty-time))))

(defun sequence-to-html (sequence)
  (labels ((contents-to-html (contents &key title subtitle number)
	     (let ((html-body (cdr (assoc :html contents))))
	       (when (or html-body title subtitle)
		 <div class="body-text sequence-text">
		   (when title
		     <h1 class="sequence-chapter">(safe (format nil "~@[~A. ~]~A" number (clean-text-to-html title :hyphenation nil)))</h1>)
		   (when subtitle
		     <div class="sequence-subtitle">(clean-text-to-html subtitle)</div>)
		   (with-html-stream-output
		       (when html-body
			 (let ((*memoized-output-stream* *html-output*)) (clean-html* html-body))))
		 </div>)))
	   (chapter-to-html (chapter)
	     (alist-bind ((title (or string null))
			   (subtitle (or string null))
			   (number (or fixnum null))
			   (contents list)
			   (posts list))
			 chapter
		<section>
		  (with-html-stream-output
		    (contents-to-html contents :title title :subtitle subtitle :number number)
		    <section>
		      (with-html-stream-output
			  (dolist (post posts)
			    (post-headline-to-html post)))
		    </section>)
		</section>)))
  (alist-bind ((sequence-id string :--id)
	       (title string)
	       (created-at string)
	       (user-id string)
	       (chapters list)
	       (contents list))
	      sequence
    (multiple-value-bind (pretty-time js-time) (pretty-time created-at)
      <article>		
        (if chapters
	  <h1 class="post-title">(safe (clean-text-to-html title :hyphenation nil))</h1>
	  <h1 class="listing"><a href=("/s/~A" sequence-id)>(safe (clean-text-to-html title :hyphenation nil))</a></h1>)
        <div class="post-meta">
          <a class=("author~{ ~A~}" (list-cond ((logged-in-userid user-id) "own-user-author")))
	     href=("/users/~A" (get-user-slug user-id))
	     data-userid=user-id>
	    (get-username user-id)
          </a>
          <div class="date" data-js-date=js-time>(progn pretty-time)</div>
        </div>
        (with-html-stream-output
	    (when chapters
	      (contents-to-html contents)
	      (dolist (chapter chapters)
	        (chapter-to-html chapter))))
      </article>))))

(defgeneric unwrap-stream (s)
  (:method ((s stream)) nil)
  (:method ((s flexi-stream)) (flexi-stream-stream s))
  (:method ((s chunga:chunked-stream)) (chunga:chunked-stream-stream s)))

(defun compare-streams (a b)
  (if (eq a b)
      t
      (or
       (if-let (u-a (unwrap-stream a))
	       (compare-streams u-a b))
       (if-let (u-b (unwrap-stream b))
	       (compare-streams a u-b)))))

(defun abort-response ()
  (throw 'abort-response nil))

(defun abort-response-if-unrecoverable (condition)
  (when (and (typep condition 'stream-error)
	     (boundp '*html-output*)
	     (compare-streams (stream-error-stream condition) *html-output*))
    (abort-response)))

(defmacro with-error-html-block (() &body body)
  "If an error occurs within BODY, write an HTML representation of the
signaled condition to *HTML-OUTPUT*."
  `(block with-error-html-block
     (handler-bind ((serious-condition (lambda (c)
					 (abort-response-if-unrecoverable c)
					 (error-to-html c)
					 (return-from with-error-html-block nil))))
       (log-conditions (progn ,@body)))))

(defun make-comment-parent-hash (comments)
  (let ((existing-comment-hash (make-hash-table :test 'equal))
        (hash (make-hash-table :test 'equal)))
    (dolist (c comments)
      (if-let (id (cdr (assoc :--id c)))
	      (setf (gethash id existing-comment-hash) t)))
    (dolist (c comments)
      (let* ((parent-id (cdr (assoc :parent-comment-id c)))
	     (old (gethash parent-id hash)))
	(setf (gethash parent-id hash) (cons c old))
        (when (and parent-id (not (gethash parent-id existing-comment-hash)))
          (let ((placeholder (alist :--id parent-id :parent-comment-id nil :deleted t)))
            (setf (gethash parent-id existing-comment-hash) t
                  (gethash nil hash) (cons placeholder (gethash nil hash)))))))
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

(defun comment-thread-to-html (out-stream emit-comment-item-fn)
  (format out-stream "<ul class=\"comment-thread\">")
  (funcall emit-comment-item-fn)
  (format out-stream "</ul>"))

(defun comment-item-to-html (out-stream comment &key extra-html-fn with-post-title level level-invert)
  (with-error-html-block ()
    (let ((c-id (cdr (assoc :--id comment)))
	  (user-id (cdr (assoc :user-id comment))))
      (format out-stream "<li id=\"comment-~A\" class=\"comment-item~{ ~A~}\">"
	      c-id
	      (list-cond
	       (t (if (let ((is-odd (or (not level) (evenp level)))) ;inverted because level counts from 0
			(if level-invert (not is-odd) is-odd))
		      "depth-odd" "depth-even"))
	       ((and *current-ignore-hash* (gethash user-id *current-ignore-hash*)) "ignored")))
      (unwind-protect
        (comment-to-html out-stream comment :with-post-title with-post-title)
        (if extra-html-fn (funcall extra-html-fn c-id))
        (format out-stream "</li>")))))

(defun comment-tree-to-html (out-stream comment-hash &key (target nil) (level (if target 1 0)) level-invert)
  (let ((comments (gethash target comment-hash)))
    (when comments
      (comment-thread-to-html out-stream
        (lambda ()
          (loop for c in comments do
	       (comment-item-to-html out-stream c
		 :level level
		 :level-invert level-invert
                 :extra-html-fn (lambda (c-id)
				  (if (and (= level 10) (gethash c-id comment-hash))
				      (format out-stream "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\" data-child-count=\"~A comment~:P\">Expand this thread</label>"
					      c-id
					      (cdr (assoc :child-count c))))
				  (comment-tree-to-html out-stream comment-hash :target c-id :level (1+ level) :level-invert level-invert)))))))))

(defun comment-chrono-to-html (out-stream comments)
  (let ((comment-hash (make-comment-parent-hash comments)) 
	(comments-sorted (sort comments #'local-time:timestamp< :key (lambda (c) (local-time:parse-timestring (cdr (assoc :posted-at c)))))))
    (comment-thread-to-html out-stream
      (lambda ()
        (loop for c in comments-sorted do
              (let* ((c-id (cdr (assoc :--id c)))
                     (new-c (acons :children (gethash c-id comment-hash) c)))
                (comment-item-to-html out-stream new-c)))))))

(defun comment-post-interleave (list &key limit offset (sort-by :date))
  (multiple-value-bind (sort-fn sort-key)
    (ecase sort-by
      (:date (values #'local-time:timestamp> (lambda (x) (local-time:parse-timestring (cdr (assoc :posted-at x))))))
      (:date-reverse (values #'local-time:timestamp< (lambda (x) (local-time:parse-timestring (cdr (assoc :posted-at x))))))
      (:score (values #'> (lambda (x) (cdr (assoc :base-score x))))))
    (let ((sorted (sort list sort-fn :key sort-key)))
      (loop for end = (if (or limit offset) (+ (or limit 0) (or offset 0)))
            for x in sorted
            for count from 0
            until (and end (>= count end))
            when (or (not offset) (>= count offset))
            collect x))))

(defun identify-item (x)
  (typecase x
    (list
     (if-let (typename (cdr (assoc :----typename x)))
	     (find-symbol (string-upcase typename) (find-package :keyword))
	     (cond
	       ((assoc :message x)
		:notification)
	       ((assoc :comment-count x)
		:post)
	       (t
		:comment))))))

(defun write-index-items-to-html (out-stream items &key need-auth (empty-message "No entries.") skip-section)
  (if items
      (dolist (x items)
	(with-error-html-block ()
	  (ecase (identify-item x)
	    (:condition
	     (error-to-html x))
	    (:notification
	     (format out-stream "<p>~A</p>" (cdr (assoc :message x))))
	    (:message
	     (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item depth-odd\">")
	     (unwind-protect
		  (conversation-message-to-html out-stream x)
	       (format out-stream "</li></ul>")))
	    (:conversation
	     (conversation-index-to-html out-stream x))
	    (:post
	     (post-headline-to-html x :need-auth need-auth :skip-section skip-section))
	    (:comment
	     (comment-thread-to-html out-stream
				     (lambda () (comment-item-to-html out-stream x :with-post-title t))))
	    (:sequence
	     (sequence-to-html x)))))
      (format out-stream "<div class=\"listing-message\">~A</div>" empty-message)))

(defun write-index-items-to-rss (out-stream items &key title need-auth)
  (let ((full-title (format nil "~@[~A - ~]~A" title (site-title *current-site*))))
    (xml-emitter:with-rss2 (out-stream :encoding "UTF-8")
      (xml-emitter:rss-channel-header full-title (site-uri *current-site*) :description full-title)
      (labels ((emit-item (item &key title link (guid (cdr (assoc :--id item))) (author (get-username (cdr (assoc :user-id item))))
                                (date (pretty-time (cdr (assoc :posted-at item)) :format local-time:+rfc-1123-format+)) body)
                          (xml-emitter:rss-item
                            title
                            :link link
                            :author author
                            :pubDate date
                            :guid guid
                            :description body)))
	(dolist (item items)
	  (ecase (identify-item item)
	    (:post
	     (let ((author (get-username (cdr (assoc :user-id item)))))
	       (emit-item item
			  :title (clean-text (format nil "~A by ~A" (cdr (assoc :title item)) author))
			  :author author
			  :link (generate-post-auth-link item nil t need-auth)
			  :body (clean-html (or (cdr (assoc :html-body (get-post-body (cdr (assoc :--id item)) :revalidate nil))) "") :post-id (cdr (assoc :--id item))))))
	    (:comment
	     (emit-item item
			:title (format nil "Comment by ~A on ~A" (get-username (cdr (assoc :user-id item))) (get-post-title (cdr (assoc :post-id item))))
			:link (generate-post-link (cdr (assoc :post-id item)) (cdr (assoc :--id item)) t)
			:body (clean-html (cdr (assoc :html-body item)))))))))))

(defparameter *html-head*
  (format nil
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta name=\"HandheldFriendly\" content=\"True\" />"))

(defparameter *extra-external-scripts* "")
(defparameter *extra-inline-scripts* "")

(defun generate-versioned-link (file)
  (format nil "~A?v=~A" file (sb-posix:stat-mtime (sb-posix:stat (format nil "www~A" file))))) 

(defun search-bar-to-html (out-stream)
  (declare (special *current-search-query*))
  (let ((query (and (boundp '*current-search-query*) (hunchentoot:escape-for-html *current-search-query*))))
    (format out-stream "<form action=\"/search\" class=\"nav-inner\"><input name=\"q\" type=\"search\" ~@[value=\"~A\"~] autocomplete=\"off\" accesskey=\"s\" title=\"Search [s]~@[&#10;Tip: Paste a ~A URL here to jump to that page.~]\"><button>Search</button></form>" query (main-site-title *current-site*))))

(defun inbox-to-html (out-stream user-slug &optional new-messages)
  (let* ((target-uri (format nil "/users/~A?show=inbox" user-slug))
         (as-link (string= (hunchentoot:request-uri*) target-uri)))
    (multiple-value-bind (nm-class nm-text)
      (if new-messages (values "new-messages" "New messages") (values "no-messages" "Inbox"))
      (format out-stream "<~:[a href=\"~A\"~;span~*~] id=\"inbox-indicator\" class=\"~A\" accesskey=\"o\" title=\"~A~:[ [o]~;~]\">~A</a>"
              as-link target-uri nm-class nm-text as-link nm-text))))

(defmethod site-nav-bars ((site site))
  '((:secondary-bar (("archive" "/archive" "Archive" :accesskey "r")
                     ("about" "/about" "About" :accesskey "t")
                     ("search" "/search" "Search" :html search-bar-to-html)
                     user-nav-item))
    (:primary-bar (("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
                   ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c")))))

(defmethod site-nav-bars ((site lesswrong-viewer-site))
  '((:secondary-bar (("archive" "/archive" "Archive" :accesskey "r")
		     ("sequences" "/library" "Sequences" :description "Sequences" :accesskey "q")
                     ("about" "/about" "About" :accesskey "t")
                     ("search" "/search" "Search" :html search-bar-to-html)
                     user-nav-item))
    (:tertiary-bar (("questions" "/index?view=questions" "Questions")
		    ("events" "/index?view=events" "Events")
		    ("shortform" "/shortform" "Shortform" :description "Latest Shortform posts")
		    ("alignment-forum" "/index?view=alignment-forum" "Alignment Forum")
		    ("alignment-forum-comments" "/recentcomments?view=alignment-forum" "AF Comments")))
    (:primary-bar (("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
                   ("featured" "/index?view=featured" "Featured" :description "Latest featured posts" :accesskey "f")
                   ("all" "/index?view=all" "All" :description "Latest posts from all sections" :accesskey "a")
                   ("meta" "/index?view=meta" "Meta" :description "Latest meta posts" :accesskey "m")
                   ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c")))))

(defmethod site-nav-bars ((site ea-forum-viewer-site))
  '((:secondary-bar (("archive" "/archive" "Archive" :accesskey "r")
                     ("about" "/about" "About" :accesskey "t")
                     ("search" "/search" "Search" :html search-bar-to-html)
                     user-nav-item))
    (:primary-bar (("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
                   ("all" "/index?view=all" "All" :description "Latest posts from all sections" :accesskey "a")
                   ("meta" "/index?view=community" "Community" :description "Latest community posts" :accesskey "m")
		   ("shortform" "/shortform" "Shortform" :description "Latest Shortform posts")
                   ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c")))))

(defun prepare-nav-bar (nav-bar current-uri)
  (list (first nav-bar)
        (map 'list (lambda (item) (if (listp item) item (funcall item current-uri)))
             (second nav-bar))))

(defun nav-item-active (item current-uri)
  (when item
    (destructuring-bind (id uri name &key description html accesskey nofollow trailing-html override-uri) item
      (declare (ignore id name description html accesskey nofollow trailing-html))
      (string= (or override-uri uri) current-uri))))

(defun nav-bar-active (nav-bar current-uri)
  (some (lambda (x) (nav-item-active x current-uri)) (second nav-bar)))

(defun nav-bar-inner (out-stream items &optional current-uri)
  (maplist (lambda (items)
             (let ((item (first items)))
               (destructuring-bind (id uri name &key description html accesskey nofollow trailing-html override-uri) item
                 (declare (ignore override-uri))
                 (let* ((item-active (nav-item-active item current-uri))
                        (nav-class (format nil "nav-item ~:[nav-inactive~;nav-current~]~:[~; nav-item-last-before-current~]"
                                           item-active (and (not item-active) (nav-item-active (cadr items) current-uri)))))
                     (format out-stream "<span id=\"nav-item-~A\" class=\"~A\" ~@[title=\"~A\"~]>"
                             id nav-class description)
                     (if html
                         (funcall html out-stream)
                         (link-if-not out-stream item-active uri "nav-inner" name :accesskey accesskey :nofollow nofollow))
                     (if trailing-html
                         (funcall trailing-html out-stream))
                     (format out-stream "</span>")))))
           items))

(defun nav-bar-outer (out-stream class nav-bar &optional current-uri)
  (format out-stream "<nav id=\"~A\" class=\"nav-bar~@[ ~A~]\">" (string-downcase (first nav-bar)) class)
  (nav-bar-inner out-stream (second nav-bar) current-uri)
  (format out-stream "</nav>"))

(defun nav-bar-to-html (out-stream class current-uri)
  (let* ((nav-bars (map 'list (lambda (x) (prepare-nav-bar x current-uri)) (site-nav-bars *current-site*)))
         (active-bar (or (find-if (lambda (x) (nav-bar-active x current-uri)) nav-bars) (car (last nav-bars))))
         (inactive-bars (remove active-bar nav-bars)))
    (dolist (bar inactive-bars)
      (nav-bar-outer out-stream (format nil "~@[~A ~]inactive-bar" class) bar current-uri))
    (nav-bar-outer out-stream (format nil "~@[~A ~]active-bar" class) active-bar current-uri)))

(defun user-nav-item (&optional current-uri)
  (if *read-only-mode*
      `("login" "/login" "Read Only Mode" :html ,(lambda (out-stream)
							 (format out-stream "<span class=\"nav-inner\" title=\"~A\">[Read Only Mode]</span>"
								 (typecase *read-only-mode*
								   (string *read-only-mode*)
								   (t *read-only-default-message*)))))
      (if-let (username (logged-in-username))
	      (let ((user-slug (encode-entities (logged-in-user-slug))))
		`("login" ,(format nil "/users/~A" user-slug) ,(plump:encode-entities username) :description "User page" :accesskey "u"
			  :trailing-html ,(lambda (out-stream) (inbox-to-html out-stream user-slug))))
	      `("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In" :accesskey "u" :nofollow t :override-uri "/login"))))

(defun sublevel-nav-to-html (options current &key default (base-uri (hunchentoot:request-uri*)) (param-name "show") (remove-params '("offset")) extra-class)
  (declare (type (or null string) extra-class))
  (let ((out-stream *html-output*))
    (format out-stream "<nav class=\"sublevel-nav~@[ ~A~]\">" extra-class)
    (loop for item in options
       do (destructuring-bind (param-value &key (text (string-capitalize param-value)) description) (if (atom item) (list item) item)
	    (let* ((param-value (string-downcase param-value))
		   (selected (string-equal current param-value))
		   (class (if selected "sublevel-item selected" "sublevel-item")))
	      (link-if-not out-stream selected (apply #'replace-query-params base-uri param-name (unless (string-equal param-value default) param-value)
						      (loop for x in remove-params nconc (list x nil)))
			   class text :title description))))
    (format out-stream "</nav>")))

(defun make-csrf-token (&optional (session-token (hunchentoot:cookie-in "session-token")) (nonce (ironclad:make-random-salt)))
  (if (typep session-token 'string) (setf session-token (base64:base64-string-to-usb8-array session-token)))
  (let ((csrf-token (concatenate '(vector (unsigned-byte 8)) nonce (ironclad:digest-sequence :sha256 (concatenate '(vector (unsigned-byte 8)) nonce session-token)))))
    (values (base64:usb8-array-to-base64-string csrf-token) csrf-token))) 

(defun check-csrf-token (csrf-token &optional (session-token (hunchentoot:cookie-in "session-token")))
  (let* ((session-token (base64:base64-string-to-usb8-array session-token))
	 (csrf-token (base64:base64-string-to-usb8-array csrf-token))
	 (correct-token (nth-value 1 (make-csrf-token session-token (subseq csrf-token 0 16)))))
    (unless (ironclad:constant-time-equal csrf-token correct-token)
      (error "CSRF check failed."))
    t)) 

(defgeneric site-stylesheets (site)
  (:method-combination append :most-specific-last)
  (:method append ((s site))
    (labels ((gen-inner (theme os)
	       (list (generate-versioned-link (format nil "/css/style~@[-~A~].~A.css" (if (and theme (> (length theme) 0)) theme) os)))))
      (let* ((ua (hunchentoot:header-in* :user-agent))
	     (theme (hunchentoot:cookie-in "theme"))
	     (os (cond ((search "Windows" ua) "windows")
		       ((search "Mac OS" ua) "mac")
		       (t "linux"))))
	(handler-case (gen-inner theme os)
	  (serious-condition () (gen-inner nil os)))))))

(defgeneric site-inline-scripts (site)
  (:method-combination append :most-specific-last)
  (:method append ((s site)) nil))

(defgeneric site-scripts (site)
  (:method-combination append :most-specific-last)
  (:method append ((s site))
	   (list (generate-versioned-link "/head.js"))))

(defgeneric site-external-scripts (site)
  (:method-combination append :most-specific-last)
  (:method append ((s site))
	   (list (generate-versioned-link "/script.js"))))

(defgeneric site-head-elements (site)
  (:method-combination append :most-specific-last)
  (:method append ((s site)) nil))

(defun html-body (out-stream fn &key title description social-description current-uri content-class robots extra-head)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
	 (csrf-token (and session-token (make-csrf-token session-token)))
	 (hide-nav-bars (truthy-string-p (hunchentoot:get-parameter "hide-nav-bars")))
	 (preview (string-equal (hunchentoot:get-parameter "format") "preview")))
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head>")
    (unless preview
      (format out-stream "<script>window.GW = { }; applicationServerKey=\"~A\"; loggedInUserId=\"~A\"; loggedInUserDisplayName=\"~A\"; loggedInUserSlug=\"~A\"; GW.useFancyFeatures=~A; ~@[GW.csrfToken=\"~A\"; ~]GW.assets="
	      (get-vapid-public-key)
	      (or (logged-in-userid) "")
	      (or (logged-in-username) "")
	      (or (logged-in-user-slug) "")
	      (if (typep *current-site* 'arbital-site) "false" "true")
	      csrf-token)
      (json:encode-json (alist "popup.svg" (generate-versioned-link "/assets/popup.svg"))
			out-stream)
      (format out-stream ";~{~A~}</script>~A"
	      (site-inline-scripts *current-site*)
	      *extra-inline-scripts*))
    (format out-stream "~A~{<link rel=\"stylesheet\" href=\"~A\">~}"
	    *html-head*
	    (site-stylesheets *current-site*))
    (generate-fonts-html-headers (site-fonts-source *current-site*))
    (format out-stream "<link rel=\"shortcut icon\" href=\"~A\">"
	    (generate-versioned-link "/assets/favicon.ico"))
    (unless preview
      (format out-stream "~{<script src=\"~A\"></script>~}"
	      (site-scripts *current-site*))
      (format out-stream "~{<script src=\"~A\" async></script>~}~A"
	      (site-external-scripts *current-site*)
	      *extra-external-scripts*))
    (format out-stream "<title>~@[~A - ~]~A</title>~@[<meta name=\"description\" content=\"~A\">~]~@[<meta name=\"robots\" content=\"~A\">~]"
	    (if title (encode-entities title))
	    (site-title *current-site*)
	    description
	    robots)
    (unless preview
      (when title
	<meta property="og:title" content=title>)
      (when social-description
	<meta property="og:description" content=social-description>
	<meta property="og:type" content="article">))
    (format out-stream "~{~A~}"
	    (site-head-elements *current-site*))
    (unless (logged-in-userid)
      <style>button.vote { display: none }</style>)
    (when *memoized-output-without-hyphens*
      ;; The browser has been detected as having bugs related to soft-hyphen characters.
      ;; But there is some hope that it could still do hyphenation by itself.
      <style>.body-text { hyphens: auto }</style>)
    (when preview
      (format out-stream "<base target='_top'>"))
    (when extra-head (funcall extra-head))
    (format out-stream "</head>")
    (unwind-protect
	 (progn
	   (format out-stream "<body><div id=\"content\"~@[ class=\"~{~A~^ ~}\"~]>"
		   (list-cond (content-class content-class)
			      (hide-nav-bars "no-nav-bars")
			      (preview "preview")))
	   (unless (or hide-nav-bars preview)
	     (nav-bar-to-html out-stream "nav-bar-top" (or current-uri (replace-query-params (hunchentoot:request-uri*) "offset" nil "sort" nil))))
	   (force-output out-stream)
	   (funcall fn))
      (format out-stream "</div></body></html>"))))

(defun replace-query-params (uri &rest params)
  (let* ((quri (quri:uri uri))
	 (old-params (quri:uri-query-params quri))
         (new-params (loop with out = old-params
                           for (param value) on params by #'cddr
                           do (if value
                                  (if-let (old-cons (assoc param out :test #'equal))
					  (setf (cdr old-cons) value)
					  (setf out (nconc out (list (cons param value)))))
				  (setf out (remove-if (lambda (x) (equal (car x) param)) out)))
                           finally (return out))))
    (if new-params 
      (setf (quri:uri-query-params quri) new-params)
      (setf (quri:uri-query quri) nil))
    (quri:render-uri quri)))

(defun pagination-nav-bars (&key offset total with-next (items-per-page (user-pref :items-per-page)))
  (if (string-equal (hunchentoot:get-parameter "format") "preview")
      (lambda (out-stream fn)
	(declare (ignore out-stream))
	(funcall fn))
      (lambda (out-stream fn)
	(labels ((pages-to-end (n) (< (+ offset (* items-per-page n)) total)))
	  (let* ((with-next (if total (pages-to-end 1) with-next))
		 (next (if (and offset with-next) (+ offset items-per-page)))
		 (prev (if (and offset (>= offset items-per-page)) (- offset items-per-page)))
		 (request-uri (hunchentoot:request-uri*))
		 (first-uri (if (and prev (> prev 0)) (replace-query-params request-uri "offset" nil)))
		 (prev-uri (if prev (replace-query-params request-uri "offset" (if (= prev 0) nil prev))))
		 (next-uri (if next (replace-query-params request-uri "offset" next)))
		 (last-uri (if (and total offset (pages-to-end 2))
			       (replace-query-params request-uri "offset" (- total (mod (- total 1) items-per-page) 1)))))
	    (if (or next prev last-uri)
		(labels ((write-item (uri class title accesskey)
			   (format out-stream "<a href=\"~A\" class=\"button nav-item-~A~:[ disabled~;~]\" title=\"~A [~A]\" accesskey=\"~A\"></a>"
				   (or uri "#") class uri title accesskey accesskey)))
		  (format out-stream "<nav id='top-nav-bar'>")
		  (write-item first-uri "first" "First page" "\\")
		  (write-item prev-uri "prev" "Previous page" "[")
		  (format out-stream "<span class='page-number'><span class='page-number-label'>Page</span> ~A</span>" (+ 1 (/ (or offset 0) items-per-page)))
		  (write-item next-uri "next" "Next page" "]")
		  (write-item last-uri "last" "Last page" "/")
		  (format out-stream "</nav>")))
	    (funcall fn)
	    (nav-bar-outer out-stream nil (list :bottom-bar
						(list-cond
						 (first-uri `("first" ,first-uri "Back to first"))
						 (prev-uri `("prev" ,prev-uri "Previous" :nofollow t))
						 (t `("top" "#top" "Back to top"))
						 (next-uri `("next" ,next-uri "Next" :nofollow t))
						 (last-uri `("last" ,last-uri "Last" :nofollow t)))))
	    (format out-stream "<script>document.querySelectorAll('#bottom-bar').forEach(bb => { bb.classList.add('decorative'); });</script>"))))))

(defun decode-json-as-hash-table (json-source)
  (let (current-hash-table current-key)
    (declare (special current-hash-table current-key))
    (json:bind-custom-vars
     (:beginning-of-object (lambda () (setf current-hash-table (make-hash-table :test 'equal)))
      :object-key (lambda (x) (setf current-key x))
      :object-value (lambda (x) (setf (gethash current-key current-hash-table) x))
      :end-of-object (lambda () current-hash-table)
      :aggregate-scope '(current-hash-table current-key))
     (json:decode-json-from-source json-source))))

(defun get-ignore-hash (&optional (user-id (logged-in-userid)))
  (if-let (ignore-json (and user-id (cache-get "user-ignore-list" user-id)))
	  (decode-json-as-hash-table ignore-json)
	  (make-hash-table :test 'equal)))

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defun call-with-emit-page (out-stream fn &key title description social-description current-uri content-class (return-code 200) robots (pagination (pagination-nav-bars)) top-nav extra-head)
  (declare (ignore return-code))
  (ignore-errors
    (log-conditions
      (html-body out-stream
                 (lambda ()
                   (when top-nav (funcall top-nav))
                   (funcall pagination out-stream fn))
                 :title title :description description :social-description social-description :current-uri current-uri :content-class content-class :robots robots :extra-head extra-head))))

(defun set-cookie (key value &key (max-age (- (expt 2 31) 1)) (path "/"))
  (hunchentoot:set-cookie key :value value :path path :max-age max-age :secure (site-secure *current-site*)))

(defun set-default-headers (return-code)
  (let ((push-option (if (hunchentoot:cookie-in "push") '("nopush"))))
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
          (hunchentoot:return-code*) return-code
          (hunchentoot:header-out :link) (format nil "~:{<~A>;rel=preload;type=~A;as=~A~@{;~A~}~:^,~}"
						 (append
						  (loop for link in (site-stylesheets *current-site*)
						     collect (list* link "text/css" "style" push-option))
						  (loop for link in (append (site-scripts *current-site*)
									    (site-external-scripts *current-site*))
						       collect (list* link "text/javascript" "script" push-option)))))
    (unless push-option (set-cookie "push" "t" :max-age (* 4 60 60)))))

(defun user-pref (key)
  (or (cdr (assoc key *current-prefs*))
      (cdr (assoc key *default-prefs*))))

(defun set-user-pref (key value)
  (assert (boundp 'hunchentoot:*reply*))
  (if value
      (setf *current-prefs* (remove-duplicates (acons key value *current-prefs*) :key #'car :from-end t))
      (setf *current-prefs* (remove key *current-prefs* :key #'car)))
  (set-cookie "prefs" (quri:url-encode (json:encode-json-to-string *current-prefs*))))

(defmacro with-response-stream ((out-stream) &body body) `(call-with-response-stream (lambda (,out-stream) ,.body)))

(defun call-with-response-stream (fn)
  (unless (eq (hunchentoot:request-method*) :head)
    (let ((*html-output* (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
      (handler-case
	  (funcall fn *html-output*)
	(serious-condition () (close *html-output*))
	(:no-error (&rest x) (declare (ignore x)) (finish-output *html-output*))))))

(defmacro emit-page ((out-stream &rest args &key (return-code 200) &allow-other-keys) &body body)
  (alexandria:once-only (return-code)
    `(progn
       (set-default-headers ,return-code)
       (with-response-stream (,out-stream)
         (call-with-emit-page ,out-stream
                              (lambda () ,@body)
                              ,@args)))))

(defun call-with-error-page (fn)
  (let* ((*current-auth-status*
	  (if-let (status-string (hunchentoot:cookie-in "lw2-status"))
		  (if (string= status-string "") nil
		      (let ((json:*identifier-name-to-key* #'json:safe-json-intern))
			(json:decode-json-from-string status-string)))))
	 (*current-prefs*
	  (if-let (prefs-string (hunchentoot:cookie-in "prefs"))
		  (let ((json:*identifier-name-to-key* 'json:safe-json-intern))
		    (ignore-errors (json:decode-json-from-string (quri:url-decode prefs-string)))))))
    (multiple-value-bind (*revalidate-default* *force-revalidate-default*)
	(cond ((ppcre:scan "(?:^|,?)\\s*(?:no-cache|max-age=0)(?:$|,)" (hunchentoot:header-in* :cache-control))
	       (values t t))
	      ((string-equal (hunchentoot:get-parameter "format") "preview")
	       (values nil nil))
	      (t
	       (values t nil)))
      (when (not *revalidate-default*)
	(setf (hunchentoot:header-out :cache-control) (format nil "public, max-age=~A" (* 5 60))))
      (with-site-context ((let ((host (or (hunchentoot:header-in* :x-forwarded-host) (hunchentoot:header-in* :host))))
			    (or (find-site host)
				(error "Unknown site: ~A" host))))
	(multiple-value-bind (*current-auth-token* *current-userid* *current-username*)
	    (if *read-only-mode*
		(values)
		(if-let
		 (auth-token
		  (if-let
		   (at (hunchentoot:cookie-in "lw2-auth-token"))
		   (if (or (string= at "") (not *current-auth-status*) (> (get-unix-time) (- (cdr (assoc :expires *current-auth-status*)) (* 60 60 24))))
		       nil at)))
		 (with-cache-readonly-transaction
		     (values
		      auth-token
		      (cache-get "auth-token-to-userid" auth-token)
		      (cache-get "auth-token-to-username" auth-token)))))
	  (let ((*current-user-slug* (and *current-userid* (get-user-slug *current-userid*)))
		(*current-ignore-hash* (get-ignore-hash))
		(*memoized-output-without-hyphens*
		 ;; Soft hyphen characters mess up middle-click paste, so try to identify whether that exists on the user's platform.
		 (let ((ua (hunchentoot:header-in* :user-agent)))
		   (or (search "X11" ua)
		       (search "Ubuntu" ua)))))
	    (catch 'abort-response
	      (handler-bind
		  ((fatal-error (lambda (condition)
				  (abort-response-if-unrecoverable condition)
				  (let ((error-html (with-output-to-string (*html-output*) (error-to-html condition))))
				    (emit-page (out-stream :title "Error" :return-code (condition-http-return-code condition) :content-class "error-page")
					       (write-string error-html out-stream)
					       (when (eq (hunchentoot:request-method*) :post)
						 <form method="post" class="error-retry-form">
						 (loop for (key . value) in (hunchentoot:post-parameters*)
						    do <input type="hidden" name=key value=value>)
						 <input type="submit" value="Retry">
						 </form>))
				    (return-from call-with-error-page)))))
		(log-conditions
		 (unless (member (hunchentoot:request-method*) '(:get :head))
		   (check-csrf-token (hunchentoot:post-parameter "csrf-token")))
		 (when-let (set-cov-pref (hunchentoot:post-parameter "set-cov-pref"))
		   (set-user-pref :hide-cov (string= set-cov-pref "1")))
		 (if (or (eq (hunchentoot:request-method*) :post)
			 (not (and (boundp '*test-acceptor*) (boundp '*hunchentoot-taskmaster*)))) ; TODO fix this hack
		     (funcall fn)
		     (sb-sys:with-deadline (:seconds (expt 1.3
							   (min (round (log 30 1.3))
								(- (hunchentoot:taskmaster-max-thread-count (symbol-value '*hunchentoot-taskmaster*))
								   (hunchentoot::accessor-requests-in-progress (symbol-value '*test-acceptor*))))))
		       (funcall fn))))))))))))

(defmacro with-error-page (&body body)
  `(call-with-error-page (lambda () ,@body)))

(defun output-form (out-stream method action id heading csrf-token fields button-label &key textarea end-html)
  (format out-stream "<form method=\"~A\" action=\"~A\" id=\"~A\"><h1>~A</h1>" method action id heading)
  (loop for (id label type . params) in fields
	do (format out-stream "<label for=\"~A\">~A:</label>" id label)
	do (cond
	     ((string= type "select")
	      (destructuring-bind (option-list &optional default) params
		(format out-stream "<select name=\"~A\">" id)
		(loop for (value label) in option-list
		      do (format out-stream "<option value=\"~A\"~:[~; selected~]>~A</option>" value (string= default value) label))
		(format out-stream "</select>")))
	     (t
	       (destructuring-bind (&optional (autocomplete "off") default) params
		 (format out-stream "<input type=\"~A\" name=\"~A\" autocomplete=\"~A\"~@[ value=\"~A\"~]>" type id autocomplete (and default (encode-entities default))))))
	do (format out-stream ""))
  (if textarea
    (destructuring-bind (ta-name ta-contents) textarea
      (format out-stream "<div class=\"textarea-container\"><textarea name=\"~A\">~A</textarea><span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span></div>" ta-name (encode-entities ta-contents))))
  (format out-stream "<input type=\"hidden\" name=\"csrf-token\" value=\"~A\"><input type=\"submit\" value=\"~A\">~@[~A~]</form>"
	  csrf-token button-label end-html))

(defun page-toolbar-to-html (&key title new-post new-conversation logout (rss t) ignore enable-push-notifications hide-cov)
  (let ((out-stream *html-output*)
	(liu (logged-in-userid)))
    (format out-stream "<div class=\"page-toolbar~@[ hide-until-init~]\">" enable-push-notifications)
    (when logout
      (format out-stream "<form method=\"post\" action=\"/logout\"><input type=\"hidden\" name=\"csrf-token\" value=\"~A\"><button class=\"logout-button button\" name=\"logout\">Log out</button></form>"
              (make-csrf-token)))
    (when ignore
      (funcall ignore))
    (when enable-push-notifications
      (format out-stream "<script>document.currentScript.outerHTML='<button id=\"enable-push-notifications\" class=\"button\" style=\"display: none\" data-enabled=\"~:[~;true~]\">~:*~:[En~;Dis~]able push notifications</button>'</script>"
	      (find-subscription *current-auth-token*)))
    (when (and new-conversation liu)
      (multiple-value-bind (text to)
        (typecase new-conversation (string (values "Send private message" new-conversation)) (t "New conversation"))
        (format out-stream "<a class=\"new-private-message button\" href=\"/conversation~@[?to=~A~]\">~A</a>"
                to text)))
    (when hide-cov
      (let ((cov-pref (user-pref :hide-cov)))
        <form method="post">
          <input type="hidden" name="csrf-token" value=(make-csrf-token)>
          <button name="set-cov-pref" value=(if cov-pref 0 1)>("~:[Hide~;Show~] coronavirus posts" cov-pref)</button>
        </form>))
    (when (and new-post liu)
      (format out-stream "<a class=\"new-post button\" href=\"/edit-post~@[?section=~A~]\" accesskey=\"n\" title=\"Create new post [n]\">New post</a>"
              (typecase new-post (string new-post) (t nil))))
    (when (and title rss)
      (format out-stream "<a class=\"rss\" rel=\"alternate\" type=\"application/rss+xml\" title=\"~A RSS feed\" href=\"~A\">RSS</a>"
              title (replace-query-params (hunchentoot:request-uri*) "offset" nil "format" "rss")))
    (format out-stream "</div>")))

(defun view-items-index (items &key section title current-uri hide-title need-auth (pagination (pagination-nav-bars)) (top-nav (lambda () (page-toolbar-to-html :title title))) (content-class "index-page") alternate-html)
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
                     ("rss" 
                      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
                      (with-response-stream (out-stream)
                        (write-index-items-to-rss out-stream items :title title)))
                     (t
                       (emit-page (out-stream :title (if hide-title nil title) :description (site-description *current-site*) :content-class content-class
                                              :current-uri current-uri :robots (if (hunchentoot:get-parameter :offset) "noindex, nofollow")
                                              :pagination pagination :top-nav top-nav)
				  (if alternate-html
				      (funcall alternate-html)
				      (write-index-items-to-html out-stream items
								 :need-auth need-auth
								 :skip-section section))))))

(defun link-if-not (stream linkp url class text &key accesskey nofollow title)
  (declare (dynamic-extent linkp url text))
  (if (not linkp)
      (format stream "<a href=\"~A\" class=\"~A\"~@[ accesskey=\"~A\"~]~:[~; rel=\"nofollow\"~]~@[ title=\"~A\"~]>~A</a>" url class accesskey nofollow title text)
      (format stream "<span class=\"~A\"~@[ title=\"~A\"~]>~A</span>" class title text)))

(defun postprocess-markdown (markdown)
  (if (typep *current-site* 'alternate-frontend-site)
      (regex-replace-body
       ((concatenate 'string (regex-replace-all "\\." (site-uri *current-site*) "\\.") "posts/([^/ ]{17})/([^/# ]*)(?:(#comment-|/comment/|/answer/)([^/ ]{17}))?")
	markdown)
       (let* ((post-id (reg 0))
	      (slug (reg 1))
	      (comment-id (reg 3))
	      (direct-comment-link (and comment-id (reg 2) (string= "/" (reg 2) :end2 1))))
	 (quri:render-uri
	  (quri:merge-uris
	   (format nil "/posts/~A/~A~[~;#~A~;?commentId=~A~]" post-id slug (if comment-id (if direct-comment-link 2 1) 0) comment-id)
	   (main-site-uri *current-site*)))))
      markdown))

(defun redirect (uri &key (type :see-other))
  (setf (hunchentoot:return-code*) (ecase type (:see-other 303) (:permanent 301))
	(hunchentoot:header-out "Location") uri))

(defmacro ignorable-multiple-value-bind ((&rest bindings) value-form &body body)
  (let (new-bindings ignores)
    (dolist (binding (reverse bindings))
      (if (eq binding '*)
	  (let ((gensym (gensym)))
	    (push gensym new-bindings)
	    (push gensym ignores))
	  (push binding new-bindings)))
    `(multiple-value-bind ,new-bindings ,value-form
	 (declare (ignore ,.ignores))
       ,@body)))

(defmacro request-method (&body method-clauses)
  (alexandria:with-gensyms (request-method)
    `(let ((,request-method (hunchentoot:request-method*)))
       (cond
	 ,.(loop for method-body in method-clauses
	      collect (destructuring-bind (method args &body inner-body) method-body
			`(,(if (eq method :get) `(member ,request-method '(:get :head)) `(eq ,request-method ,method))
			   ,(make-binding-form (mapcar (lambda (x) (append (ensure-list x) `(:request-type ,method))) args)
					       inner-body))))))))

(defmacro define-page (name path-specifier additional-vars &body body)
  (labels ((make-lambda (args)
             (loop for a in args
                   collect (if (atom a) a (first a)))))
   (multiple-value-bind (path-specifier-form path-bindings-wrapper specifier-vars)
    (if (stringp path-specifier)
	(values path-specifier #'identity)
	(destructuring-bind (specifier-type specifier-body &rest specifier-args) path-specifier
	  (ecase specifier-type
	    (:function
	      (values `(lambda (r) (funcall ,specifier-body (hunchentoot:request-uri r)))
		      (if specifier-args
                          (lambda (body) `(ignorable-multiple-value-bind ,(make-lambda specifier-args) (funcall ,specifier-body (hunchentoot:request-uri*)) ,body))
                          #'identity)
                      specifier-args))
            (:regex
              (let ((fn `(lambda (r) (ppcre:scan-to-strings ,specifier-body (hunchentoot:request-uri r)))))
                (values fn
                        (lambda (body)
                          (alexandria:with-gensyms (result-vector)
                            `(let ((,result-vector (nth-value 1 (funcall ,fn hunchentoot:*request*))))
                               (declare (type simple-vector ,result-vector)) 
                               (let
                                 ,(loop for v in (make-lambda specifier-args) as x from 0 collecting `(,v (if (> (length ,result-vector) ,x) (aref ,result-vector ,x)))) 
                                 ,body))))
                        specifier-args))))))
     `(hunchentoot:define-easy-handler (,name :uri ,path-specifier-form) ()
	(with-error-page
	    (block nil
	      ,(funcall path-bindings-wrapper
			(make-binding-form (append (mapcar (lambda (x) (append (ensure-list x) '(:passthrough t))) specifier-vars) additional-vars)
					   body))))))))

(define-component sort-widget (&key (sort-options '((:new :description "Sort by date posted")
						    (:hot :description "Sort by time-weighted score")
						    (:active :description "Sort by date posted or last comment")
						    (:old :description "Sort by date posted, oldest first")))
				    (pref :default-sort) (param-name "sort") (html-class "sort"))
  (:http-args '((sort :real-name param-name :member (mapcar (lambda (x) (if (listp x) (first x) x)) sort-options))
		(sortedby :real-name "sortedBy" :type string)))
  (if sortedby
      (progn
	(renderer (out-stream) (progn out-stream nil)) ;todo: support declarations so we don't need this dirty trick
	sortedby)
      (let ((sort-string (if sort (string-downcase sort))))
	(if sort-string
	    (set-user-pref :default-sort sort-string))
	(renderer ()
		  (sublevel-nav-to-html sort-options
					(user-pref pref)
					:param-name param-name
					:extra-class html-class))
	(or sort-string (user-pref pref)))))

(define-component view-index ()
  (:http-args '((view :member '(:all :new :frontpage :featured :meta :community :alignment-forum :questions :nominations :reviews :events) :default :frontpage)
		before after
		(offset :type fixnum)
		(limit :type fixnum :default (user-pref :items-per-page))))
  (when (eq view :new) (redirect (replace-query-params (hunchentoot:request-uri*) "view" "all" "all" nil) :type :permanent) (return))
  (component-value-bind ((sort-string sort-widget))
    (multiple-value-bind (posts total)
	(get-posts-index :view (string-downcase view) :before before :after after :offset offset :limit (1+ limit) :sort sort-string
			 :hide-tags (if (user-pref :hide-cov) (list (get-slug-tagid "coronavirus"))))
      (let ((page-title (format nil "~@(~A posts~)" view)))
	(renderer ()
		  (view-items-index (firstn posts limit)
				    :section view :title page-title :hide-title (eq view :frontpage)
				    :pagination (pagination-nav-bars :offset (or offset 0) :total total :with-next (and (not total) (> (length posts) limit)))
				    :content-class (format nil "index-page ~(~A~)-index-page" view)
				    :top-nav (lambda ()
					       (page-toolbar-to-html :title page-title
								     :new-post (if (eq view :meta) "meta" t)
								     :hide-cov (typep *current-site* 'lesswrong-viewer-site))
					       (if (member view '(:frontpage :all))
						   (funcall sort-widget)))))))))

(defmacro route-component (name lambda-list &rest args)
  `(lambda ,lambda-list
     (with-error-page
	 (component-value-bind ((() (,name ,@args)))
			       (when ,name
				 (funcall ,name))))))

(define-route 'forum-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-index ()))

(define-route 'forum-site 'standard-route :name 'view-index :uri "/index" :handler (route-component view-index ()))

(hunchentoot:define-easy-handler
    (view-site-routes
     :uri (lambda (req)
	    (declare (ignore req))
	    (with-site-context ((let ((host (or (hunchentoot:header-in* :x-forwarded-host) (hunchentoot:header-in* :host))))
				  (or (find-site host)
				      (error "Unknown site: ~A" host))))
	      (call-route-handler *current-site* (hunchentoot:script-name*)))))
    nil)

(define-page view-post "/post" ((id :required t))
  (redirect (generate-post-link id) :type :permanent))

(define-page view-post-lw1-link (:function #'match-lw1-link) ()
  (redirect (convert-lw1-link (hunchentoot:request-uri*)) :type :permanent))

(define-page view-post-ea1-link (:function #'match-ea1-link) ()
  (redirect (convert-ea1-link (hunchentoot:request-uri*)) :type :permanent))

(define-page view-post-lw2-slug-link (:function #'match-lw2-slug-link) ()
  (redirect (convert-lw2-slug-link (hunchentoot:request-uri*)) :type :see-other))

(define-page view-post-lw2-sequence-link (:function #'match-lw2-sequence-link) ()
  (redirect (convert-lw2-sequence-link (hunchentoot:request-uri*)) :type :see-other))

(define-page view-feed "/feed" ()
  (redirect "/?format=rss" :type :permanent))

(define-page view-allposts "/allPosts" ()
  (redirect (format nil "/index~@[?~A~]" (hunchentoot:query-string*)) :type :see-other))

(define-page view-nominations "/nominations" ()
  (redirect "/index?view=nominations" :type :see-other))

(define-page view-reviews "/reviews" ()
  (redirect "/index?view=reviews" :type :see-other))

(define-page view-review-voting "/reviewVoting" ()
  (redirect "https://www.lesswrong.com/reviewVoting" :type :see-other))

(define-page view-coronavirus-link-database "/coronavirus-link-database" ()
  (redirect "https://www.lesswrong.com/coronavirus-link-database" :type :see-other))

(defun post-comment (post-id-real &key shortform)
  (request-method
   (:post (text answer nomination nomination-review af post-id parent-answer-id parent-comment-id edit-comment-id retract-comment-id unretract-comment-id delete-comment-id)
     (let ((lw2-auth-token *current-auth-token*))
       (assert lw2-auth-token)
       (let* ((post-id (or post-id-real post-id))
	      (question (when post-id (cdr (assoc :question (get-post-body post-id :auth-token lw2-auth-token)))))
	      (new-comment-id
	       (cond
		 (text
		  (let ((comment-data
			 (list-cond
			  (t :body (postprocess-markdown text))
			  ((not (or edit-comment-id (and shortform (not parent-comment-id)))) :post-id post-id)
			  (parent-comment-id :parent-comment-id parent-comment-id)
			  (answer :answer t)
			  (nomination :nominated-for-review "2018")
			  (nomination-review :reviewing-for-review "2018")
			  (parent-answer-id :parent-answer-id parent-answer-id)
			  (af :af t)
			  ((and shortform (not parent-comment-id)) :shortform t))))
		    (if edit-comment-id
			(prog1 edit-comment-id
			  (do-lw2-comment-edit lw2-auth-token edit-comment-id comment-data))
			(do-lw2-comment lw2-auth-token comment-data))))
		 (retract-comment-id
		  (do-lw2-comment-edit lw2-auth-token retract-comment-id '((:retracted . t))))
		 (unretract-comment-id
		  (do-lw2-comment-edit lw2-auth-token unretract-comment-id '((:retracted . nil))))
		 (delete-comment-id
		  (do-lw2-comment-remove lw2-auth-token delete-comment-id :reason "Comment deleted by its author.")
		  nil))))
	 (when post-id
	   (ignore-errors
	     (get-post-comments post-id :force-revalidate t)
	     (when question
	       (get-post-answers post-id :force-revalidate t))))
	 (when text
	   (cache-put "comment-markdown-source" new-comment-id text)
	   (redirect (quri:render-uri
		      (quri:merge-uris (quri:make-uri :fragment (format nil "comment-~A" new-comment-id))
				       (hunchentoot:request-uri*))))))))))

(define-page view-post-lw2-link (:function #'match-lw2-link post-id comment-id * comment-link-type)
                                (need-auth
                                 chrono
				 (show-comments :real-name "comments" :type boolean :default t)
				 (format :type string))
  (request-method
   (:get ()
     (let* ((lw2-auth-token *current-auth-token*)
	    (preview (string-equal format "preview"))
	    (show-comments (and (not preview) show-comments)))
       (labels ((output-comments (out-stream id comments target &key overcomingbias-sort)
		  (labels ((output-comments-inner ()
			     (with-error-html-block ()
			       (if target
				   (comment-thread-to-html out-stream
							   (lambda ()
							     (comment-item-to-html
							      out-stream
							      target
							      :level-invert preview
							      :extra-html-fn (lambda (c-id)
									       (let ((*comment-individual-link* nil))
										 (comment-tree-to-html out-stream (make-comment-parent-hash comments)
												       :target c-id
												       :level-invert preview))))))
				   (if comments
				       (progn #|<div class="comments-empty-message">(safe (pretty-number (length comments) id))</div>|#
					 (if chrono
					     (comment-chrono-to-html out-stream comments)
					     (let ((parent-hash (make-comment-parent-hash comments)))
					       (when overcomingbias-sort
						 (setf (gethash nil parent-hash)
						       (sort (gethash nil parent-hash) #'local-time:timestamp< :key (lambda (c) (local-time:parse-timestring (cdr (assoc :posted-at c)))))))
					       (comment-tree-to-html out-stream parent-hash))))
				       <div class="comments-empty-message">("No ~As." id)</div>)))))
		    (if preview
			(output-comments-inner)
			(progn (format out-stream "<div id=\"~As\" class=\"comments\">" id)
			       (unless target
				 <script>initializeCommentControls\(\)</script>)
			       (output-comments-inner)
			       (format out-stream "</div>"))))))
	 (multiple-value-bind (post title condition)
	     (handler-case (nth-value 0 (get-post-body post-id :auth-token (and need-auth lw2-auth-token)))
	       (serious-condition (c) (values nil "Error" c))
	       (:no-error (post) (values post (cdr (assoc :title post)) nil)))
	   (labels ((extra-head ()
		      (when-let (canonical-source (and (not comment-id)
						       (cdr (assoc :canonical-source post))))
			<link rel="canonical" href=canonical-source>)
		      <script>postId=(with-html-stream-output (json:encode-json post-id *html-output*))</script>
		      <script>alignmentForumPost=(if (cdr (assoc :af post)) "true" "false")</script>)
		    (retrieve-individual-comment (comment-thread-type)
		      (let* ((comments (case comment-thread-type
					 (:comment (get-post-comments post-id))
					 (:answer (get-post-answers post-id))))
			     (target-comment (find comment-id comments :key (lambda (c) (cdr (assoc :--id c))) :test #'string=)))
			(values comments target-comment))))
	     (if comment-id
		 (let ((comment-thread-type (if (string= comment-link-type "answer") :answer :comment)))
		   (multiple-value-bind (comments target-comment) (retrieve-individual-comment comment-thread-type)
		     (unless target-comment
		       ;; If the comment was not found, try as an answer, or vice versa.
		       (setf comment-thread-type (if (eq comment-thread-type :comment) :answer :comment)
			     (values comments target-comment) (retrieve-individual-comment comment-thread-type))
		       (unless target-comment
			 (error (make-condition 'lw2-not-found-error))))
		     (let* ((*comment-individual-link* t)
			    (display-name (get-username (cdr (assoc :user-id target-comment))))
			    (verb-phrase (cond
					   ((and (eq comment-thread-type :answer)
						 (not (cdr (assoc :parent-comment-id target-comment))))
					    "answers")
					   (t "comments on"))))
		       (emit-page (out-stream :title (format nil "~A ~A ~A" display-name verb-phrase title)
					      :content-class "individual-thread-page comment-thread-page"
					      :social-description (when-let (x (cdr (assoc :html-body target-comment))) (extract-excerpt x))
					      :extra-head #'extra-head)
				  (unless preview
				    (format out-stream "<h1 class=\"post-title\">~A ~A <a href=\"~A\">~A</a></h1>"
					    (encode-entities display-name)
					    verb-phrase
					    (generate-post-link post-id)
					    (clean-text-to-html title :hyphenation nil)))
				  (output-comments out-stream "comment" comments target-comment)))))
		 (let ((post-sequences (get-post-sequences post-id)))
		   (emit-page (out-stream :title title
					  :content-class (format nil "post-page comment-thread-page~{ ~A~}"
								 (list-cond ((cdr (assoc :question post)) "question-post-page")
									    (post-sequences "in-sequence")
									    ((not show-comments) "no-comments")))
					  :social-description (when-let (x (cdr (assoc :html-body post))) (extract-excerpt x))
					  :extra-head #'extra-head)
			      (cond
				(condition
				 (error-explanation-case (error-to-html condition)
							 (lw2-not-allowed-error
							  <p>This probably means the post has been deleted or moved back to the author's drafts.</p>)))
				(t
				 (post-body-to-html post)))
			      (when (and lw2-auth-token (equal (logged-in-userid) (cdr (assoc :user-id post))))
				(format out-stream "<div class=\"post-controls\"><a class=\"edit-post-link button\" href=\"/edit-post?post-id=~A\" accesskey=\"e\" title=\"Edit post [e]\">Edit post</a></div>"
					(cdr (assoc :--id post))))
			      (post-nav-links post post-sequences)
			      (when show-comments
				(force-output out-stream)
				(with-error-html-block ()
				  ;; Temporary hack to support nominations
				  (let ((real-comments (get-post-comments post-id))
					(answers (when (cdr (assoc :question post))
						   (get-post-answers post-id)))
					(nominations-eligible (and (typep *current-backend* 'backend-lw2)
								   (cdr (assoc :posted-at post))
								   (let ((ts (local-time:parse-timestring (cdr (assoc :posted-at post)))))
								     (and (local-time:timestamp> ts (load-time-value (local-time:parse-timestring "2018-01-01")))
									  (local-time:timestamp< ts (load-time-value (local-time:parse-timestring "2019-01-01"))))))))
				    (labels ((top-level-property (comment property)
					       (or (cdr (assoc property comment))
						   (cdr (assoc property (cdr (assoc :top-level-comment comment)))))))
				      (multiple-value-bind (normal-comments nominations reviews)
					  (loop for comment in real-comments
					     if (top-level-property comment :nominated-for-review)
					     collect comment into nominations
					     else if (top-level-property comment :reviewing-for-review)
					     collect comment into reviews
					     else
					     collect comment into normal-comments
					     finally (return (values normal-comments nominations reviews)))
					(loop for (name comments) in (list-cond (nominations-eligible
										 (list "nomination" nominations))
										(nominations-eligible
										 (list "review" reviews))
										((cdr (assoc :question post))
										 (list "answer" answers))
										(t
										 (list "comment" normal-comments)))
					   do (with-error-html-block ()
						(output-comments out-stream name comments nil :overcomingbias-sort (cdr (assoc :comment-sort-order post))))))))))))))))))
   (:post ()
     (post-comment post-id))))

(defparameter *edit-post-template* (compile-template* "edit-post.html"))

(define-page view-edit-post "/edit-post" (title url section tags post-id link-post)
  (request-method
    (:get ()
     (let* ((csrf-token (make-csrf-token))
            (post-body (if post-id (get-post-body post-id :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
            (section (or section (loop for (sym . sec) in '((:draft . "drafts") (:meta . "meta") (:frontpage-date . "frontpage"))
                                       if (cdr (assoc sym post-body)) return sec
                                       finally (return "all")))))
       (emit-page (out-stream :title (if post-id "Edit Post" "New Post") :content-class "edit-post-page")
                  (render-template* *edit-post-template* out-stream
                                    :csrf-token csrf-token
                                    :title (cdr (assoc :title post-body))
                                    :url (cdr (assoc :url post-body))
				    :question (cdr (assoc :question post-body))
				    :tags-supported (typep *current-backend* 'backend-accordius)
				    :tags (when (and post-id (typep *current-backend* 'backend-accordius)) (do-wl-rest-query (format nil "posts/~a/update_tagset/" post-id) '()))
                                    :post-id post-id
                                    :section-list (loop for (name desc) in '(("all" "All") ("meta" "Meta") ("drafts" "Drafts"))
						     collect (alist :name name :desc desc :selected (string= name section)))
				    :lesswrong-misc (typep *current-backend* 'backend-lw2-misc-features)
				    :submit-to-frontpage (if post-id (cdr (assoc :submit-to-frontpage post-body)) t)
                                    :markdown-source (or (and post-id (cache-get "post-markdown-source" post-id)) (cdr (assoc :html-body post-body)) "")))))
    (:post (text question submit-to-frontpage)
     (let ((lw2-auth-token *current-auth-token*)
           (url (if (string= url "") nil url)))
       (assert lw2-auth-token)
       (let* ((post-data
	       (list-cond
		(t :body (postprocess-markdown text))
		(t :title title)
		(link-post :url url)
		(t :meta (string= section "meta"))
		(t :draft (string= section "drafts"))
		((typep *current-backend* 'backend-lw2-misc-features) :submit-to-frontpage (and submit-to-frontpage t))
		((not post-id) :question (and question t))))
	      (post-unset
	       (list-cond
		((not link-post) :url t)))
	      (new-post-data
	       (if post-id
		   (do-lw2-post-edit lw2-auth-token post-id post-data post-unset)
		   (do-lw2-post lw2-auth-token post-data)))
	      (new-post-id (cdr (assoc :--id new-post-data))))
	 (assert new-post-id)
	 (when (typep *current-backend* 'backend-accordius)
	   (do-wl-rest-mutate :post
	     (format nil "posts/~a/update_tagset/" post-id)
	     (alist "tags" tags)
	     lw2-auth-token))
	 (cache-put "post-markdown-source" new-post-id text)
	 (ignore-errors (get-post-body post-id :force-revalidate t))
	 (redirect (if (cdr (assoc :draft post-data))
		       (concatenate 'string (generate-post-link new-post-data) "?need-auth=y")
		       (generate-post-link new-post-data))))))))

(hunchentoot:define-easy-handler (view-karma-vote :uri "/karma-vote") ()
  (with-error-page
      (let ((auth-token *current-auth-token*))
	(with-response-stream (out-stream)
	  (request-method
	    (:get (post-id shortform (offset :type fixnum :default 0))
	      (json:encode-json-alist
	       (list-cond
		(post-id "postVote" (get-post-vote post-id auth-token))
		(post-id "commentVotes" (alist-hash-table (delete-if (lambda (x) (null (cdr x))) (get-post-comments-votes post-id auth-token))))
		(shortform "commentVotes" (alist-hash-table (delete-if (lambda (x) (null (cdr x))) (get-shortform-votes auth-token :offset offset))))
		(t "alignmentForumAllowed"
		   (member "alignmentForum" (cdr (assoc :groups (get-user :user-id (logged-in-userid)))) :test #'string=)))
	       out-stream))
	    (:post (target target-type vote-type)
	      (multiple-value-bind (points vote-type vote-data) (do-lw2-vote auth-token target target-type vote-type)
		(json:encode-json
		 (alist-bind (af af-base-score) vote-data
			     (list (vote-buttons points :as-text t :af-score (and af af-base-score)) vote-type))
		 out-stream))))))))

(hunchentoot:define-easy-handler (view-check-notifications :uri "/check-notifications") (format)
  (with-error-page
      (if *current-auth-token*
	  (let ((notifications-status (check-notifications (logged-in-userid) *current-auth-token* :full (string= format "push"))))
	    (json:encode-json-to-string notifications-status)))))

(hunchentoot:define-easy-handler (view-ignore-user :uri "/ignore-user") ((csrf-token :request-type :post) (target-id :request-type :post) (state :request-type :post) return)
  (with-error-page
      (check-csrf-token csrf-token)
      (let ((user-id (logged-in-userid)))
	(unless user-id (error "Not logged in."))
	(with-cache-transaction
	    (let ((ignore-hash (get-ignore-hash user-id)))
	      (if (string= state "ignore")
		  (setf (gethash target-id ignore-hash) t)
		  (remhash target-id ignore-hash))
	      (cache-put "user-ignore-list" user-id (json:encode-json-to-string ignore-hash)))))
      (when return
	(redirect return))))

(defun standalone-comment-controls ()
  <div class="posting-controls standalone with-markdown-editor" onsubmit="disableBeforeUnload();">
    <form method="post" id="conversation-form" class="aligned-form">
      <div class="textarea-container">
	<textarea name="text" oninput="enableBeforeUnload();"></textarea>
	<span class="markdown-reference-link">You can use <a href="http://commonmark.org/help/" target="_blank">Markdown</a> here.</span>
	<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-help-button">Help</button>
	<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-exit-button">Exit</button>
      </div>
      <div>
	<input name="csrf-token" value=(make-csrf-token) type="hidden">
	<input value="Submit" type="submit">
      </div>
    </form>
  </div>)

(define-component view-comments-index (index-type)
  (:http-args '((offset :type fixnum)
		(limit :type fixnum)
		(view :member '(nil :alignment-forum))))
  (request-method
   (:get ()
     (let ((want-total (not (or (typep *current-backend* 'backend-lw2) (typep *current-backend* 'backend-ea-forum)))) ; LW2/EAF can't handle total queries. TODO: handle this in backend.
	   (shortform (eq index-type :shortform)))
       (multiple-value-bind (title query-view top-nav)
	   (cond
	     (shortform (values "Shortform" "shortform" (if (logged-in-userid) 'standalone-comment-controls)))
	     (t (values (case view (:alignment-forum "Alignment Forum recent comments") (t "Recent comments")) "allRecentComments" nil)))
	 (multiple-value-bind (recent-comments total)
	     (if (or (not (eq index-type :recent-comments)) offset limit view (/= (user-pref :items-per-page) 20))
		 (let ((*use-alignment-forum* (eq view :alignment-forum)))
		   (lw2-graphql-query (lw2-query-string :comment :list
							(remove nil (alist :view query-view
									   :limit (or limit (user-pref :items-per-page)) :offset offset)
								:key #'cdr)
							:context (if shortform :shortform :index)
							:with-total want-total)))
		 (get-recent-comments :with-total want-total))
	   (renderer ()
	     (view-items-index recent-comments
			       :title title
			       :content-class (if shortform "shortform-index-page comment-thread-page")
			       :pagination (pagination-nav-bars :offset (or offset 0) :with-next (not want-total) :total (if want-total total))
			       :top-nav (lambda () (page-toolbar-to-html :title title) (when top-nav (funcall top-nav)))
			       :alternate-html (if (eq index-type :shortform)
						   (lambda ()
						     <div class="comments">
						       (comment-tree-to-html *html-output*
									     (make-comment-parent-hash
									      (flatten-shortform-comments recent-comments)))
						     </div>))))))))
   (:post ()
     (post-comment nil :shortform t))))

(define-route 'forum-site 'standard-route :name 'view-recent-comments :uri "/recentcomments" :handler (route-component view-comments-index () :recent-comments))
(define-route 'shortform-site 'standard-route :name 'view-shortform :uri "/shortform" :handler (route-component view-comments-index () :shortform))

(define-component view-tag (slug)
  (:http-args '())
  (let ((posts (get-tag-posts slug)))
    (renderer ()
      (view-items-index posts
			:title "Tag page"
			:content-class "tag-index-page"))))

(define-route 'forum-site 'regex-route :name 'view-tag :regex "/tag/([^/?]+)" :handler (route-component view-tag (slug) slug))

(define-component view-tags-index ()
  (:http-args '())
  (let ((tags (lw2-graphql-query (lw2-query-string :tag :list (alist :view "allTagsAlphabetical") :fields '(:name :slug :post-count (:description :html))))))
    (renderer ()
      (emit-page (out-stream :title "All tags")
        <div class="tags-index page-list-index">
	  <ul>
	    (dolist (tag tags)
	      (alist-bind ((name simple-string) (slug simple-string) (post-count (or null integer)) (description list)) tag
	        <li>
	          <a href=("/tag/~A" slug)>(progn name) \((or post-count 0)\)</a>
	          (when-let (description-html (cdr (assoc :html description)))
		    <div>(with-html-stream-output (let ((*memoized-output-stream* out-stream)) (clean-html* description-html)))</div>)
		</li>))
	  </ul>
	</div>))))

(define-route 'forum-site 'standard-route :name 'view-tags-index :uri "/tags" :handler (route-component view-tags-index ()))

(delete-easy-handler 'view-recent-comments)

(hunchentoot:define-easy-handler (view-push-register :uri "/push/register") ()
  (with-error-page
      (let* ((data (call-with-safe-json (lambda ()
					  (json:decode-json-from-string
					   (hunchentoot:raw-post-data :force-text t :external-format :utf-8)))))
	     (subscription (cdr (assoc :subscription data)))
	     (cancel (cdr (assoc :cancel data))))
	(cond
	  (subscription
	   (make-subscription *current-auth-token*
			      (cdr (assoc :endpoint subscription))
			      (cdr (assoc :expires *current-auth-status*)))
	   (send-notification (cdr (assoc :endpoint subscription)) :ttl 120))
	  (cancel
	   (delete-subscription *current-auth-token*)))
	nil)))

(hunchentoot:define-easy-handler (view-inbox-redirect :uri "/push/go-inbox") ()
  (with-error-page
      (redirect (format nil "/users/~A?show=inbox" *current-user-slug*))))

(define-page view-user (:regex "^/users/(.*?)(?:$|\\?)|^/user" user-slug) (id
                                                                             (offset :type fixnum :default 0)
                                                                             (show :member '(:all :posts :comments :drafts :conversations :inbox) :default :all)
                                                                             (sort :member '(:top :new :old) :default :new))
             (let* ((auth-token (if (eq show :inbox) *current-auth-token*))
		    (user-info
		     (let ((ui (get-user (cond (user-slug :user-slug) (id :user-id)) (or user-slug id) :auth-token auth-token)))
		       (if (and (not (cdr (assoc :deleted ui))) (cdr (assoc :--id ui)))
			   ui
			   (error (make-condition 'lw2-user-not-found-error)))))
		    (user-id (cdr (assoc :--id user-info)))
		    (own-user-page (logged-in-userid user-id))
                    (display-name (if user-slug (cdr (assoc :display-name user-info)) user-id))
                    (show-text (if (not (eq show :all)) (string-capitalize show)))
                    (title (format nil "~A~@['s ~A~]" display-name show-text))
                    (sort-type (case sort (:top :score) (:new :date) (:old :date-reverse))))
	       (multiple-value-bind (items total)
                 (case show
                   (:posts
		    (get-user-page-items user-id :posts :offset offset :limit (+ 1 (user-pref :items-per-page)) :sort-type sort-type))
                   (:comments
		    (get-user-page-items user-id :comments :offset offset :limit (+ 1 (user-pref :items-per-page)) :sort-type sort-type))
                   (:drafts
		    (get-user-page-items user-id :posts :drafts t :offset offset :limit (+ 1 (user-pref :items-per-page)) :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
		   (:conversations
                     (let ((conversations
                             (lw2-graphql-query (lw2-query-string :conversation :list
                                                                  (alist :view "userConversations" :limit (+ 1 (user-pref :items-per-page)) :offset offset :user-id user-id)
                                                                  :fields '(:--id :created-at :title (:participants :display-name :slug) :----typename))
                                                :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                       (lw2-graphql-query-map
                         (lambda (c)
                           (lw2-query-string* :message :total (alist :view "messagesConversation" :conversation-id (cdr (assoc :--id c)))))
                         conversations
                         :postprocess (lambda (c result)
                                        (acons :messages-total result c))
                         :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                   (:inbox
		    (error-explanation-case
                     (prog1
                       (let ((notifications (get-notifications :user-id user-id :offset offset :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                             (last-check (ignore-errors (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info))))))
			 (labels ((check-new (key obj)
				    (if (ignore-errors (local-time:timestamp< last-check (local-time:parse-timestring (cdr (assoc key obj)))))
					(acons :highlight-new t obj)
					obj))
				  (check-replied (comment)
				    (let* ((post-id (cdr (assoc :post-id comment)))
					   (comment-id (cdr (assoc :--id comment)))
					   (comments (funcall (if (or (cdr (assoc :answer comment))
								      (cdr (assoc :parent-answer-id comment)))
								  'get-post-answers
								  'get-post-comments)
							      post-id
							      :revalidate nil))
					   (reply-comment (find-if (lambda (c)
								     (and (string= (cdr (assoc :parent-comment-id c)) comment-id)
									  (string= (cdr (assoc :user-id c)) user-id)))
								   comments)))
				      (if reply-comment
					  (acons :replied (list post-id (cdr (assoc :--id reply-comment))) comment)
					  comment))))
			   (lw2-graphql-query-map
                             (lambda (n)
                               (alexandria:switch ((cdr (assoc :document-type n)) :test #'string=)
                                                  ("comment"
                                                   (lw2-query-string* :comment :single
                                                                      (alist :document-id (cdr (assoc :document-id n)))
                                                                      :context :index))
                                                  ("post"
                                                   (lw2-query-string* :post :single (alist :document-id (cdr (assoc :document-id n)))))
                                                  ("message"
                                                   (lw2-query-string* :message :single (alist :document-id (cdr (assoc :document-id n)))
                                                                      :fields *messages-index-fields*))
                                                  (t
                                                    (values n t))))
                             notifications
                             :postprocess (lambda (n result)
                                            (if result
						(funcall (if (string= (cdr (assoc :document-type n)) "comment") #'check-replied #'identity)
							 (check-new
							  (alexandria:switch ((cdr (assoc :document-type n)) :test #'string=)
									     ("comment" :posted-at)
									     ("post" :posted-at)
									     ("message" :created-at))
							  result))
						n))
			     :auth-token auth-token)))
		       (do-user-edit
			 (hunchentoot:cookie-in "lw2-auth-token")
			 user-id
			 (alist :last-notifications-check (timestamp-to-graphql (local-time:now)))))
		     (lw2-not-allowed-error
		      <p>This may mean your login token has expired or become invalid. You can try <a href="/login">logging in again</a>.</p>)))
		   (t
		    (get-user-page-items user-id :both :offset offset :limit (+ 1 (user-pref :items-per-page)) :sort-type sort-type)))
                 (let ((with-next (> (length items) (+ (if (eq show :all) offset 0) (user-pref :items-per-page))))
                       (interleave (if (eq show :all) (comment-post-interleave items :limit (user-pref :items-per-page) :offset (if (eq show :all) offset nil) :sort-by sort-type) (firstn items (user-pref :items-per-page))))) ; this destructively sorts items
                   (view-items-index interleave :title title
                                     :content-class (format nil "user-page~@[ ~A-user-page~]~:[~; own-user-page~]" (string-downcase show) own-user-page)
                                     :current-uri (format nil "/users/~A" user-slug)
                                     :section :personal
                                     :pagination (pagination-nav-bars :offset offset :total total :with-next (if (not total) with-next))
                                     :need-auth (eq show :drafts) :section (if (eq show :drafts) "drafts" nil)
                                     :top-nav (lambda ()
                                                (page-toolbar-to-html :title title
								      :enable-push-notifications (eq show :inbox)
                                                                      :rss (not (member show '(:drafts :conversations :inbox)))
                                                                      :new-post (if (eq show :drafts) "drafts" t)
                                                                      :new-conversation (if own-user-page t user-slug)
								      :ignore (if (and (logged-in-userid) (not own-user-page))
										  (lambda ()
										    (let ((ignored (gethash user-id *current-ignore-hash*)))
										      <form method="post" action="/ignore-user">
										        <button class=("button ~A" (if ignored "unignore-button" "ignore-button"))>(if ignored "Unignore user" "Ignore user")</button>
										        <input type="hidden" name="csrf-token" value=(make-csrf-token)>
										        <input type="hidden" name="target-id" value=user-id>
											<input type="hidden" name="state" value=(if ignored "unignore" "ignore")>
											<input type="hidden" name="return" value=(hunchentoot:request-uri*)>
										      </form>)))
                                                                      :logout own-user-page)
						(alist-bind ((actual-id string :--id)
							     (actual-slug string :slug)
							     (karma (or null fixnum))
							     (af-karma (or null fixnum)))
							    user-info
						  <h1 class="page-main-heading"
						      (when (not own-user-page)
						        (format nil "data-~:[~;anti-~]~:*kibitzer-redirect=~:[/users/~*~A~;/user?id=~A~]"
								user-slug actual-id actual-slug))>
						      (progn display-name)
						      (let ((full-name (get-user-full-name user-id)))
							(when (and user-slug (stringp full-name) (> (length full-name) 0))
								  <span class="user-full-name">\((progn full-name)\)</span>))
						  </h1>
						  <div class="user-stats">
						    Karma:
						    <span class="karma-type">
						      <span class="karma-total">(if user-slug (pretty-number (or karma 0)) "##")</span>(if af-karma " (LW),")
						    </span>\ 
						    (when af-karma
						      <span class="karma-type">
						        <span class="karma-total af-karma-total">(if user-slug (pretty-number (or af-karma 0)) "##")</span> \(AF\)
						      </span>)
						  </div>)
						(sublevel-nav-to-html `(:all :posts :comments
									     ,@(if own-user-page
										   '(:drafts :conversations :inbox)))
								      show
								      :default :all)
						(when (member show '(:all :posts :comments))
						  (sublevel-nav-to-html '(:new :top :old)
                                                                        sort
                                                                        :default :new
                                                                        :param-name "sort"
                                                                        :extra-class "sort"))))))))

(defparameter *conversation-template* (compile-template* "conversation.html"))

(define-page view-conversation "/conversation" (id to subject)
  (request-method
   (:get ()
     (cond
       ((and id to) (error "This is an invalid URL."))
       (id
	(multiple-value-bind (conversation messages)
	    (get-conversation-messages id (hunchentoot:cookie-in "lw2-auth-token"))
	  (let ((conversation (rectify-conversation conversation)))
	    (view-items-index (nreverse messages) :content-class "conversation-page" :need-auth t :title (encode-entities (cdr (assoc :title conversation)))
			      :top-nav (lambda () (render-template* *conversation-template* *html-output*
								    :conversation conversation :csrf-token (make-csrf-token)))))))
       (t
	(emit-page (out-stream :title "New conversation" :content-class "conversation-page")
		   (render-template* *conversation-template* out-stream
				     :to to
				     :csrf-token (make-csrf-token))))))
   (:post ((text :required t))
     (let* ((id (or id
		    (let ((participant-ids (list (logged-in-userid) (cdar (lw2-graphql-query (lw2-query-string :user :single (alist :slug to) :fields '(:--id)))))))
		      (do-create-conversation (hunchentoot:cookie-in "lw2-auth-token") (alist :participant-ids participant-ids :title subject))))))
       (do-create-message (hunchentoot:cookie-in "lw2-auth-token") id text)
       (redirect (format nil "/conversation?id=~A" id))))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body
              (handler-case (nth-value 1 (cl-markdown:markdown (cdr (assoc :body item)) :stream nil))
                (serious-condition () "[Error while processing search result]")))
        item))

(define-page view-search "/search" ((q :required t))
  (let ((*current-search-query* q)
        (link (convert-any-link* q)))
    (declare (special *current-search-query*))
    (if link
        (redirect link)
        (multiple-value-bind (posts comments) (lw2-search-query q)
          (view-items-index (nconc (map 'list (lambda (p) (if (cdr (assoc :comment-count p)) p (cons (cons :comment-count 0) p))) posts)
                                   (map 'list #'search-result-markdown-to-html comments))
                            :content-class "search-results-page" :current-uri "/search"
                            :title (format nil "~@[~A - ~]Search" q))))))

(define-page view-login "/login" (return cookie-check
                                         (login-username :request-type :post) (login-password :request-type :post)
                                         (signup-username :request-type :post) (signup-email :request-type :post) (signup-password :request-type :post) (signup-password2 :request-type :post))
  (labels
    ((emit-login-page (&key error-message)
       (let ((csrf-token (make-csrf-token)))
         (emit-page (out-stream :title "Log in" :current-uri "/login" :content-class "login-page" :robots "noindex, nofollow")
                    (when error-message
                      (format out-stream "<div class=\"error-box\">~A</div>" error-message)) 
                    (with-outputs (out-stream) "<div class=\"login-container\">")
                    (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "login-form" "Log in" csrf-token
                                 '(("login-username" "Username" "text" "username")
                                   ("login-password" "Password" "password" "current-password"))
                                 "Log in"
                                 :end-html "<a href=\"/reset-password\">Forgot password</a>")
                    (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "signup-form" "Create account" csrf-token
                                 '(("signup-username" "Username" "text" "username")
                                   ("signup-email" "Email" "text" "email")
                                   ("signup-password" "Password" "password" "new-password")
                                   ("signup-password2" "Confirm password" "password" "new-password"))
                                 "Create account")
                    (if-let (main-site-title (main-site-title *current-site*))
			    (format out-stream "<div class=\"login-tip\"><span>Tip:</span> You can log in with the same username and password that you use on ~A~:*. Creating an account here also creates one on ~A.</div>"
				    main-site-title))
		    (format out-stream "</div>"))))
     (finish-login (username user-id auth-token error-message &optional expires)
       (cond
         (auth-token
           (set-cookie "lw2-auth-token" auth-token :max-age (and expires (+ (- expires (get-unix-time)) (* 24 60 60))))
           (if expires (set-cookie "lw2-status" (json:encode-json-to-string (alist :expires expires))))
           (cache-put "auth-token-to-userid" auth-token user-id)
           (cache-put "auth-token-to-username" auth-token username)
           (redirect (if (and return (ppcre:scan "^/[^/]" return)) return "/")))
         (t
          (emit-login-page :error-message error-message)))))
    (cond
      ((not (or cookie-check (hunchentoot:cookie-in "session-token")))
        (set-cookie "session-token" (base64:usb8-array-to-base64-string (ironclad:make-random-salt)))
        (redirect (format nil "/login?~@[return=~A&~]cookie-check=y" (if return (url-rewrite:url-encode return))))) 
      (cookie-check
        (if (hunchentoot:cookie-in "session-token")
            (redirect (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))))
            (emit-page (out-stream :title "Log in" :current-uri "/login")
                       (format out-stream "<h1>Enable cookies</h1><p>Please enable cookies in your browser and <a href=\"/login~@[?return=~A~]\">try again</a>.</p>" (if return (url-rewrite:url-encode return)))))) 
      (login-username
        (cond
          ((or (string= login-username "") (string= login-password "")) (emit-login-page :error-message "Please enter a username and password"))
	  ((lw2.dnsbl:dnsbl-check (hunchentoot:real-remote-addr)) (emit-login-page :error-message "Your IP address is blacklisted."))
          (t (multiple-value-call #'finish-login login-username (do-login "username" login-username login-password))))) 
      (signup-username
        (cond
          ((not (every (lambda (x) (not (string= x ""))) (list signup-username signup-email signup-password signup-password2)))
           (emit-login-page :error-message "Please fill in all fields"))
          ((not (string= signup-password signup-password2))
           (emit-login-page :error-message "Passwords do not match"))
	  ((lw2.dnsbl:dnsbl-check (hunchentoot:real-remote-addr)) (emit-login-page :error-message "Your IP address is blacklisted."))
          (t (multiple-value-call #'finish-login signup-username (do-lw2-create-user signup-username signup-email signup-password)))))
      (t
       (emit-login-page))))) 

(define-page view-logout "/logout" ()
  (request-method
   (:post ()
     (set-cookie "lw2-auth-token" "" :max-age 0)
     (redirect "/"))))

(defparameter *reset-password-template* (compile-template* "reset-password.html"))

(define-page view-reset-password "/reset-password" ((email :request-type :post) (reset-link :request-type :post) (password :request-type :post) (password2 :request-type :post))
  (labels ((emit-rpw-page (&key message message-type step)
             (let ((csrf-token (make-csrf-token)))
               (emit-page (out-stream :title "Reset password" :content-class "reset-password" :robots "noindex, nofollow")
                          (render-template* *reset-password-template* out-stream
                                            :csrf-token csrf-token
                                            :reset-link reset-link
                                            :message message
                                            :message-type message-type
                                            :step step)))))
    (cond
      (email
        (multiple-value-bind (ret error)
          (do-lw2-forgot-password email)
          (declare (ignore ret))
          (if error
              (emit-rpw-page :step 1 :message error :message-type "error")
              (emit-rpw-page :step 1 :message "Password reset email sent." :message-type "success"))))
      (reset-link
        (ppcre:register-groups-bind (reset-token) ("(?:reset-password/|^)([^/#]+)$" reset-link)
                                    (cond
                                      ((not reset-token)
                                       (emit-rpw-page :step 2 :message "Invalid password reset link." :message-type "error"))
                                      ((not (string= password password2))
                                       (emit-rpw-page :step 2 :message "Passwords do not match." :message-type "error"))
                                      (t
                                       (multiple-value-bind (user-id auth-token error-message) (do-lw2-reset-password reset-token password)
                                         (declare (ignore user-id auth-token))
                                         (cond
                                           (error-message (emit-rpw-page :step 2 :message error-message :message-type "error"))
                                           (t
                                            (with-error-page (emit-page (out-stream :title "Reset password" :content-class "reset-password")
                                                                        (format out-stream "<h1>Password reset complete</h1><p>You can now <a href=\"/login\">log in</a> with your new password.</p>"))))))))))
      (t
       (emit-rpw-page)))))

(define-page view-sequences "/library"
                            ((view :member '(:featured :community) :default :featured))
  (let ((sequences
	 (lw2-graphql-query
	  (lw2-query-string :sequence :list
			    (alist :view (case view
					   (:featured "curatedSequences")
					   (:community "communitySequences")))
			    :fields '(:--id :created-at :user-id :title :----typename)))))
    (view-items-index
     sequences
     :title "Sequences Library"
     :content-class "sequences-page"
     :current-uri "/library"
     :top-nav (lambda ()
		(sublevel-nav-to-html '(:featured :community)
				      view
				      :default :featured
				      :param-name "view"
				      :extra-class "sequences-view")))))

(define-page view-sequence (:regex "^/s(?:equences)?/([^/?#]+)(?:/?$|\\?)" sequence-id) ()
  (let ((sequence (get-sequence sequence-id)))
    (alist-bind ((title string))
		sequence
      (emit-page (out-stream
		  :title title
		  :content-class "sequence-page")
		 (sequence-to-html sequence)))))

(define-page view-archive (:regex "^/archive(?:/(\\d{4})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))"
                           (year :type (mod 10000))
                           (month :type (integer 1 12))
                           (day :type (integer 1 31)))
             ((offset :type fixnum :default 0))
  (local-time:with-decoded-timestamp (:day current-day :month current-month :year current-year :timezone local-time:+utc-zone+) (local-time:now)
    (local-time:with-decoded-timestamp (:day earliest-day :month earliest-month :year earliest-year :timezone local-time:+utc-zone+) (earliest-post-time)
      (labels ((url-elements (&rest url-elements)
                 (declare (dynamic-extent url-elements))
                 (format nil "/~{~A~^/~}" url-elements))
	       (archive-nav ()
		 (let ((out-stream *html-output*))
		   (with-outputs (out-stream) "<div class=\"archive-nav\"><div class=\"archive-nav-years\">")
		   (link-if-not out-stream (not (or year month day)) (url-elements "archive") "archive-nav-item-year" "All") 
		   (loop for y from earliest-year to current-year
		      do (link-if-not out-stream (eq y year) (url-elements "archive" y) "archive-nav-item-year" y))
		   (format out-stream "</div>")
		   (when year
		     (format out-stream "<div class=\"archive-nav-months\">")
		     (link-if-not out-stream (not month) (url-elements "archive" year) "archive-nav-item-month" "All") 
		     (loop for m from (if (= (or year current-year) earliest-year) earliest-month 1) to (if (= (or year current-year) current-year) current-month 12)
			do (link-if-not out-stream (eq m month) (url-elements "archive" (or year current-year) m) "archive-nav-item-month" (elt local-time:+short-month-names+ m)))
		     (format out-stream "</div>"))
		   (when month
		     (format out-stream "<div class=\"archive-nav-days\">")
		     (link-if-not out-stream (not day) (url-elements "archive" year month) "archive-nav-item-day" "All")
		     (loop for d from (if (and (= (or year current-year) earliest-year) (= (or month current-month) earliest-month)) earliest-day 1)
			to (if (and (= (or year current-year) current-year) (= (or month current-month) current-month)) current-day (local-time:days-in-month (or month current-month) (or year current-year)))
			do (link-if-not out-stream (eq d day) (url-elements "archive" (or year current-year) (or month current-month) d) "archive-nav-item-day" d))
		     (format out-stream "</div>")) 
		   (format out-stream "</div>"))))
	(multiple-value-bind (posts total)
	  (lw2-graphql-query (lw2-query-string :post :list
					       (alist :view (if day "new" "top") :limit 51 :offset offset
						      :after (if (and year (not day)) (format nil "~A-~A-~A" (or year earliest-year) (or month 1) (or day 1)))
                                                      :before (if year (format nil "~A-~A-~A" (or year current-year) (or month 12)
                                                                               (or day (local-time:days-in-month (or month 12) (or year current-year))))))))
          (emit-page (out-stream :title "Archive" :current-uri "/archive" :content-class "archive-page"
                                 :top-nav #'archive-nav
                                 :pagination (pagination-nav-bars :items-per-page 50 :offset offset :total total :with-next (if total nil (> (length posts) 50))))
                     (write-index-items-to-html out-stream (firstn posts 50) :empty-message "No posts for the selected period.")))))))

(define-page view-about "/about" ()
  (emit-page (out-stream :title "About" :current-uri "/about" :content-class "about-page")
             (alexandria:with-input-from-file (in-stream "www/about.html" :element-type '(unsigned-byte 8))
                                              (alexandria:copy-stream in-stream out-stream))))

(hunchentoot:define-easy-handler (view-versioned-resource :uri (lambda (r)
                                                                 (multiple-value-bind (file content-type)
                                                                   #.(labels ((defres (uri content-type)
                                                                                `(,uri (values (concatenate 'string "www" ,uri) ,content-type))))
                                                                       (concatenate 'list
                                                                                    '(alexandria:switch ((hunchentoot:script-name r) :test #'string=))
                                                                                    (loop for system in '("mac" "windows" "linux") nconc
                                                                                      (loop for theme in '(nil "dark" "grey" "ultramodern" "zero" "brutalist" "rts" "classic" "less")
                                                                                            collect (defres (format nil "/css/style~@[-~A~].~A.css" theme system) "text/css")))
                                                                                    (loop for (uri content-type) in
										      '(("/arbital.css" "text/css")
											("/head.js" "text/javascript")
											("/script.js" "text/javascript")
											("/assets/favicon.ico" "image/x-icon")
											("/assets/telegraph.jpg" "image/jpeg")
											("/assets/popup.svg" "image/svg+xml"))
                                                                                      collect (defres uri content-type))))
                                                                   (when file
                                                                     (when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
                                                                       (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
                                                                     (hunchentoot:handle-static-file file content-type)
                                                                     t))))
                                 nil)
