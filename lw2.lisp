(uiop:define-package #:lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:djula
	#:lw2-viewer.config #:lw2.utils #:lw2.lmdb #:lw2.backend #:lw2.links #:lw2.clean-html #:lw2.login #:lw2.context #:lw2.sites #:lw2.components #:lw2.html-reader)
  (:unintern
    #:define-regex-handler #:*fonts-stylesheet-uri* #:generate-fonts-link
    #:user-nav-bar #:*primary-nav* #:*secondary-nav* #:*nav-bars*
    #:begin-html #:end-html))

(in-package #:lw2-viewer) 

(named-readtables:in-readtable html-reader)

(add-template-directory (asdf:system-relative-pathname "lw2-viewer" "templates/"))

(define-cache-database "auth-token-to-userid" "auth-token-to-username" "comment-markdown-source" "post-markdown-source")

(defvar *current-auth-token*)
(defvar *current-userid*)
(defvar *current-username*)
(defvar *current-user-slug*)

(defvar *read-only-mode* nil)
(defvar *read-only-default-message* "Due to a system outage, you cannot log in or post at this time.")

(defparameter *default-prefs* (alist :items-per-page 20 :default-sort "new"))
(defvar *current-prefs* nil)

(defun logged-in-userid (&optional is-userid)
  (let ((current-userid *current-userid*))
    (if is-userid
        (string= current-userid is-userid)
        current-userid))) 

(defun logged-in-username ()
  *current-username*)

(defun logged-in-user-slug ()
  *current-user-slug*)

(defun pretty-time (timestring &key format)
  (let ((time (local-time:parse-timestring timestring)))
  (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	  (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-number (number &optional object)
  (let ((str (coerce (format nil "~:D~@[<span> ~A~P</span>~]" number object number) '(vector character))))
    (if (eq (aref str 0) #\-)
      (setf (aref str 0) #\MINUS_SIGN))
    str))

(defun generate-post-auth-link (post &optional comment-id absolute need-auth)
  (if need-auth
      (concatenate 'string (generate-post-link post comment-id absolute) "?need-auth=y")
      (generate-post-link post comment-id absolute)))

(defun clean-lw-link (url)
  (when url
    (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com")))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun post-section-to-html (post &key skip-section)
  (alist-bind ((user-id string)
	       (frontpage-date (or null string))
	       (curated-date (or null string))
	       (meta boolean)
	       (af boolean)
	       (draft boolean))
	      post
	      (multiple-value-bind (class title href)
		  (cond (af (if (eq skip-section :alignment-forum) nil (values "alignment-forum" "View Alignment Forum posts" "/index?view=alignment-forum")))
					; show alignment forum even if skip-section is t
			((eq skip-section t) nil)
			(draft nil)
			(curated-date (if (eq skip-section :featured) nil (values "featured" "View Featured posts" "/index?view=featured")))
			(frontpage-date (if (eq skip-section :frontpage) nil (values "frontpage" "View Frontpage posts" "/")))
			(meta (if (eq skip-section :meta) nil (values "meta" "View Meta posts" "/index?view=meta")))
			(t (if (eq skip-section :personal) nil (values "personal" (format nil "View posts by ~A" (get-username user-id)) (format nil "/users/~A?show=posts" (get-user-slug user-id))))))
		<a class=("post-section ~A" class) title=title href=href></a>)))

(defun post-headline-to-html (post &key skip-section need-auth)
  (alist-bind ((post-id string :--id)
               (title string)
               (user-id string)
               (url (or null string))
               (posted-at string)
               (base-score fixnum)
               (comment-count (or null fixnum))
               (page-url (or null string))
               (word-count (or null fixnum))
               (frontpage-date (or null string))
               (curated-date (or null string))
               (meta boolean)
               (af boolean)
	       (question boolean)
               (vote-count (or null fixnum))
               (draft boolean))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      <h1 class=("listing~{ ~A~}" (list-cond
				   (url "link-post-listing")
				   (question "question-post-listing")
				   ((logged-in-userid user-id) "own-post-listing")))>
        (if url <a href=(convert-any-link (string-trim " " url))>&#xf0c1;</a>)
	<a href=(generate-post-auth-link post nil nil need-auth)>
	  (if question <span class="post-type-prefix">[Question] </span>)
	  (safe (clean-text-to-html title))
	</a>
	(if (logged-in-userid user-id) <a class="edit-post-link button" href=("/edit-post?post-id=~A" post-id)</a>)
      </h1>
      <div class="post-meta">
        <a class=("author~{ ~A~}" (list-cond ((logged-in-userid user-id) "own-user-author")))
	   href=("/users/~A" (get-user-slug user-id))
	   data-userid=user-id>
	  (get-username user-id)
        </a>
        <div class="date" data-js-date=js-time>(progn pretty-time)</div>
        <div class="karma">
          <span class="karma-value" title=(votes-to-tooltip vote-count)>(safe (pretty-number base-score "point"))</span>
        </div>
        <a class="comment-count" href=("~A#comments" (generate-post-link post))>(safe (pretty-number (or comment-count 0) "comment"))</a>
        (if word-count <span class="read-time" title=(safe (pretty-number word-count "word"))>(max 1 (round word-count 300))<span> min read</span></span>)
        (if page-url <a class="lw2-link" href=(clean-lw-link page-url)>(main-site-abbreviation *current-site*)<span> link</span></a>)
        (with-html-stream-output (post-section-to-html post :skip-section skip-section))
        (if url <div class="link-post-domain">("(~A)" (puri:uri-host (puri:parse-uri (string-trim " " url))))</div>)
      </div>)))

(defun post-body-to-html (post)
  (alist-bind ((post-id string :--id)
               (title string)
               (user-id string)
               (url (or null string))
               (posted-at string)
               (base-score fixnum)
	       (tags (or null list))
               (comment-count (or null fixnum))
               (page-url (or null string))
               (frontpage-date (or null string))
               (curated-date (or null string))
               (meta boolean)
               (draft boolean)
               (af boolean)
	       (question boolean)
               (vote-count (or null fixnum))
               (html-body (or null string)))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      <div class=("post~{ ~A~}" (list-cond
				 (url "link-post")
				 (question "question-post")))>
        <h1 class="post-title">
          (if question <span class="post-type-prefix">[Question] </span>)
          (safe (clean-text-to-html title :hyphenation nil))
        </h1>
        <div class="post-meta">
          <a class=("author~{ ~A~}" (list-cond
				     ((logged-in-userid user-id) "own-user-author")))
             href=("/users/~A" (get-user-slug user-id))
             data-userid=user-id>
            (get-username user-id)
          </a>
          <div class="date" data-js-date=js-time>(progn pretty-time)</div>
          <div class="karma" data-post-id=post-id>
            <span class="karma-value" title=(votes-to-tooltip vote-count)>(safe (pretty-number base-score "point"))</span>
          </div>
          <a class="comment-count" href="#comments">(safe (pretty-number (or comment-count 0) "comment"))</a>
          (if page-url <a class="lw2-link" href=(clean-lw-link page-url)>(main-site-abbreviation *current-site*)<span> link</span></a>)
          (with-html-stream-output (post-section-to-html post))
	  (when tags
	    <ul id="tags">
	      (dolist (tag tags) (alist-bind ((text string)) tag <li><a href=("/tags/~A" text)>(progn text)</a></li>))
	    </ul>)
	</div>
	<div class="post-body">
          (if url <p><a class="link-post-link" href=(convert-any-link (string-trim " " url))>Link post</a></p>)
          (with-html-stream-output
	      (write-sequence (clean-html* (or html-body "") :with-toc t :post-id post-id) *html-output*))
        </div>
      </div>)))

(defparameter *comment-individual-link* nil)

(defun comment-to-html (out-stream comment &key with-post-title)
  (if (or (cdr (assoc :deleted comment)) (cdr (assoc :deleted-public comment)))
      (format out-stream "<div class=\"comment deleted-comment\"><div class=\"comment-meta\"><span class=\"deleted-meta\">[ ]</span></div><div class=\"comment-body\">[deleted]</div></div>")
      (alist-bind ((comment-id string :--id)
                   (user-id string)
                   (posted-at string)
                   (highlight-new boolean)
                   (post-id string)
                   (base-score fixnum)
                   (page-url (or null string))
                   (parent-comment list)
                   (parent-comment-id (or null string))
                   (child-count (or null fixnum))
                   (children list)
                   (vote-count (or null fixnum))
		   (retracted boolean)
		   (answer boolean)
		   (parent-answer-id (or null string))
                   (html-body string))
                  comment
                  (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
		    <div class=("comment~{ ~A~}"
				(list-cond
				 ((and (logged-in-userid user-id)
				       (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000)))
				  "just-posted-comment")
				 (highlight-new "comment-item-highlight")
				 (retracted "retracted")))>
		      <div class="comment-meta">
			<a class=("author~:[~; own-user-author~]" (logged-in-userid user-id))
			   href=("/users/~A" (encode-entities (get-user-slug user-id)))
			   data-userid=user-id>
			  (get-username user-id)
			</a>
			<a class="date" href=(generate-post-link post-id comment-id) data-js-date=js-time> (safe pretty-time) </a>
			<div class="karma">
			  <span class="karma-value" title=(votes-to-tooltip vote-count)> (safe (pretty-number base-score "point")) </span>
			</div>
			<a class="permalink" href=("~A/~A/~A"
						   (generate-post-link post-id)
						   (cond ((or answer parent-answer-id) "answer") (t "comment"))
						   comment-id)
			   title="Permalink"></a>
			(with-html-stream-output
			  (when page-url
			    <a class="lw2-link" href=(clean-lw-link page-url) title=(main-site-abbreviation *current-site*)></a>)
		          (if with-post-title
			      <div class="comment-post-title">
			        (with-html-stream-output
				    (when parent-comment
				      (alist-bind ((user-id string)
						   (post-id string)
						   (parent-id string :--id))
						  parent-comment
					<span class="comment-in-reply-to">in reply to:
					  <a href=("/users/~A" (get-user-slug user-id))
					     class=("inline-author~:[~; own-user-author~]" (logged-in-userid user-id))
					     data-userid=(progn user-id)>
					     (get-username user-id)</a>'s
					  <a href=(generate-post-link post-id parent-id)>comment</a>
					  (progn " ")
			                </span>)))
				  <span class="comment-post-title2">on: <a href=(generate-post-link post-id)>(safe (clean-text-to-html (get-post-title post-id)))</a></span>
				</div>
			    (when parent-comment-id
			      (if *comment-individual-link*
				  <a class="comment-parent-link" href=(progn parent-comment-id) title="Parent"></a>
				  <a class="comment-parent-link" href=("#comment-~A" parent-comment-id)>Parent</a>)))
			  (when children
			    <div class="comment-child-links">
			      Replies:
			      (with-html-stream-output
				(dolist (child children)
				  (alist-bind ((comment-id string)
					       (user-id string))
					      child
				    <a href=("#comment-~A" comment-id)>(">~A" (get-username user-id))</a>)))
			    </div>)
			  <div class="comment-minimize-button"
			       data-child-count=(progn child-count)>
			  </div>)
			</div>
			<div class="comment-body" (safe ("~@[ data-markdown-source=\"~A\"~]"
							 (if (logged-in-userid user-id)
							     (encode-entities
							      (or (cache-get "comment-markdown-source" comment-id)
								  html-body)))))>
			  (with-html-stream-output (write-sequence (clean-html* html-body) out-stream))
                        </div>
		      </div>))))

(defun postprocess-conversation-title (title)
  (if (or (null title) (string= title ""))
      "[Untitled conversation]"
      title))

(defun conversation-message-to-html (out-stream message)
  (alist-bind ((user-id string)
               (created-at string)
               (highlight-new boolean)
               (conversation list)
               (content list)
               (html-body (or string null)))
    message
    (multiple-value-bind (pretty-time js-time) (pretty-time created-at)
      (format out-stream "<div class=\"comment private-message~A\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <span class=\"date\" data-js-date=\"~A\">~A</span><div class=\"comment-post-title\">Private message in: <a href=\"/conversation?id=~A\">~A</a></div></div><div class=\"comment-body\">"
              (if highlight-new " comment-item-highlight" "")
              (encode-entities (get-user-slug user-id))
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              (encode-entities (cdr (assoc :--id conversation)))
              (encode-entities (postprocess-conversation-title (cdr (assoc :title conversation))))))
    (if html-body
        (write-sequence (clean-html* html-body) out-stream)
        (format out-stream "~{<p>~A</p>~}" (loop for block in (cdr (assoc :blocks content)) collect (encode-entities (cdr (assoc :text block))))))
    (format out-stream "</div></div>")))

(defun conversation-index-to-html (out-stream conversation)
  (alist-bind ((conversation-id string :--id)
               (title (or null string))
               (created-at (or null string))
               (participants list)
               (messages-total fixnum))
    conversation
    (multiple-value-bind (pretty-time js-time) (if created-at (pretty-time created-at) (values "[Error]" 0))
      (format out-stream "<h1 class=\"listing\"><a href=\"/conversation?id=~A\">~A</a></h1><div class=\"post-meta\"><div class=\"conversation-participants\"><ul>~:{<li><a href=\"/users/~A\">~A</a></li>~}</ul></div><div class=\"messages-count\">~A</div><div class=\"date\" data-js-date=\"~A\">~A</div></div>"
              (encode-entities conversation-id)
              (encode-entities (postprocess-conversation-title title))
              (loop for p in participants
                    collect (list (encode-entities (cdr (assoc :slug p))) (encode-entities (cdr (assoc :display-name p)))))
              (pretty-number messages-total "message")
              js-time
              pretty-time))))

(defun error-to-html (out-stream condition)
  (format out-stream "<div class=\"gw-error\"><h1>Error</h1><p>~A</p></div>"
          (encode-entities (princ-to-string condition))))

(defmacro with-error-html-block ((out-stream) &body body)
  "If an error occurs within BODY, write an HTML representation of the
signaled condition to OUT-STREAM."
  `(handler-case
     (progn ,@body)
     (serious-condition (c) (error-to-html ,out-stream c))))

(defun make-comment-parent-hash (comments)
  (let ((existing-comment-hash (make-hash-table :test 'equal))
        (hash (make-hash-table :test 'equal)))
    (dolist (c comments)
      (alexandria:if-let (id (cdr (assoc :--id c)))
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

(defun comment-item-to-html (out-stream comment &key extra-html-fn)
  (with-error-html-block (out-stream)
    (let ((c-id (cdr (assoc :--id comment))))
      (format out-stream "<li id=\"comment-~A\" class=\"comment-item\">" c-id)
      (unwind-protect
        (comment-to-html out-stream comment)
        (if extra-html-fn (funcall extra-html-fn c-id))
        (format out-stream "</li>")))))

(defun comment-tree-to-html (out-stream comment-hash &optional (target nil) (level 0))
  (let ((comments (gethash target comment-hash)))
    (when comments
      (comment-thread-to-html out-stream
        (lambda ()
          (loop for c in comments do
                (comment-item-to-html out-stream c
                  :extra-html-fn (lambda (c-id)
                                   (if (and (= level 10) (gethash c-id comment-hash))
                                       (format out-stream "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\" data-child-count=\"~A comment~:P\">Expand this thread</label>"
                                               c-id
                                               (cdr (assoc :child-count c))))
                                   (comment-tree-to-html out-stream comment-hash c-id (1+ level))))))))))

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
      (:score (values #'> (lambda (x) (cdr (assoc :base-score x))))))
    (let ((sorted (sort list sort-fn :key sort-key)))
      (loop for end = (if (or limit offset) (+ (or limit 0) (or offset 0)))
            for x in sorted
            for count from 0
            until (and end (>= count end))
            when (or (not offset) (>= count offset))
            collect x))))

(defun write-index-items-to-html (out-stream items &key need-auth (empty-message "No entries.") skip-section)
  (if items
      (dolist (x items)
        (with-error-html-block (out-stream)
          (cond
            ((typep x 'condition)
             (error-to-html out-stream x))
            ((assoc :message x)
             (format out-stream "<p>~A</p>" (cdr (assoc :message x))))
            ((string= (cdr (assoc :----typename x)) "Message")
             (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item\">")
             (unwind-protect
               (conversation-message-to-html out-stream x)
               (format out-stream "</li></ul>")))
            ((string= (cdr (assoc :----typename x)) "Conversation")
             (conversation-index-to-html out-stream x))
            ((assoc :comment-count x)
             (post-headline-to-html x :need-auth need-auth :skip-section skip-section))
            (t
             (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item\" id=\"comment-~A\">" (cdr (assoc :--id x)))
             (unwind-protect
               (comment-to-html out-stream x :with-post-title t)
               (format out-stream "</li></ul>"))))))
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
          (if (assoc :comment-count item)
            (let ((author (get-username (cdr (assoc :user-id item)))))
              (emit-item item
                         :title (clean-text (format nil "~A by ~A" (cdr (assoc :title item)) author))
                         :author author
                         :link (generate-post-auth-link item nil t need-auth)
                         :body (clean-html (or (cdr (assoc :html-body (get-post-body (cdr (assoc :--id item)) :revalidate nil))) "") :post-id (cdr (assoc :--id item)))))
            (emit-item item
                       :title (format nil "Comment by ~A on ~A" (get-username (cdr (assoc :user-id item))) (get-post-title (cdr (assoc :post-id item))))
                       :link (generate-post-link (cdr (assoc :post-id item)) (cdr (assoc :--id item)) t)
                       :body (clean-html (cdr (assoc :html-body item))))))))))

(defparameter *fonts-stylesheet-uris*
  '("https://fonts.greaterwrong.com/?fonts=InconsolataGW,CharterGW,ConcourseGW,Whitney,MundoSans,SourceSansPro,Raleway,ProximaNova,TiredOfCourier,AnonymousPro,InputSans,InputSansNarrow,InputSansCondensed,GaramondPremierPro,TriplicateCode,TradeGothic,NewsGothicBT,Caecilia,SourceSerifPro,SourceCodePro"
    "https://fonts.greaterwrong.com/?fonts=BitmapFonts,FontAwesomeGW&base64encode=1"))
;(defparameter *fonts-stylesheet-uris* '("https://fonts.greaterwrong.com/?fonts=*"))

(defvar *fonts-redirect-data* nil)
(sb-ext:defglobal *fonts-redirect-lock* (make-mutex))
(sb-ext:defglobal *fonts-redirect-thread* nil)

(defun generate-fonts-links ()
  (let ((current-time (get-unix-time)))
    (labels ((get-redirects (uri-list)
               (loop for request-uri in uri-list collect
                     (multiple-value-bind (body status headers uri)
                       (drakma:http-request request-uri :method :head :close t :redirect nil :additional-headers (alist :referer (site-uri (first *sites*)) :accept "text/css,*/*;q=0.1"))
                       (declare (ignore body uri))
                       (let ((location (cdr (assoc :location headers))))
                         (if (and (typep status 'integer) (< 300 status 400) location)
                             location
                             nil)))))
             (update-redirects ()
               (handler-case
                 (let* ((new-redirects (get-redirects *fonts-stylesheet-uris*))
                        (new-redirects (loop for new-redirect in new-redirects
                                             for original-uri in *fonts-stylesheet-uris*
                                             collect (if new-redirect (quri:render-uri (quri:merge-uris (quri:uri new-redirect) (quri:uri original-uri))) original-uri))))
                   (with-mutex (*fonts-redirect-lock*) (setf *fonts-redirect-data* (list *fonts-stylesheet-uris* new-redirects current-time)
                                                             *fonts-redirect-thread* nil))
                   new-redirects)
                 (serious-condition () *fonts-stylesheet-uris*)))
             (ensure-update-thread ()
               (with-mutex (*fonts-redirect-lock*)
                 (or *fonts-redirect-thread*
                     (setf *fonts-redirect-thread* (make-thread #'update-redirects :name "fonts redirect update"))))))
      (destructuring-bind (&optional base-uris redirect-uris timestamp) (with-mutex (*fonts-redirect-lock*) *fonts-redirect-data*)
        (if (and (eq base-uris *fonts-stylesheet-uris*) timestamp)
          (progn
            (if (>= current-time (+ timestamp 60))
                (ensure-update-thread))
            (or redirect-uris *fonts-stylesheet-uris*))
          (update-redirects))))))

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
                     user-nav-item))
    (:primary-bar (("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
                   ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c")))))

(defmethod site-nav-bars ((site lesswrong-viewer-site))
  '((:secondary-bar (("archive" "/archive" "Archive" :accesskey "r")
                     ("about" "/about" "About" :accesskey "t")
                     ("search" "/search" "Search" :html search-bar-to-html)
                     user-nav-item))
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
  (format out-stream "<div id=\"~A\" class=\"nav-bar~@[ ~A~]\">" (string-downcase (first nav-bar)) class)
  (nav-bar-inner out-stream (second nav-bar) current-uri)
  (format out-stream "</div>"))

(defun nav-bar-to-html (out-stream &optional current-uri)
  (let* ((nav-bars (map 'list (lambda (x) (prepare-nav-bar x current-uri)) (site-nav-bars *current-site*)))
         (active-bar (or (find-if (lambda (x) (nav-bar-active x current-uri)) nav-bars) (car (last nav-bars))))
         (inactive-bars (remove active-bar nav-bars)))
    (dolist (bar inactive-bars)
      (nav-bar-outer out-stream "inactive-bar" bar current-uri))
    (nav-bar-outer out-stream "active-bar" active-bar current-uri)))

(defun user-nav-item (&optional current-uri)
  (if *read-only-mode*
      `("login" "/login" "Read Only Mode" :html ,(lambda () (format nil "<span class=\"nav-inner\" title=\"~A\">[Read Only Mode]</span>"
                                                                    (typecase *read-only-mode*
                                                                      (string *read-only-mode*)
                                                                      (t *read-only-default-message*)))))
      (alexandria:if-let (username (logged-in-username))
          (let ((user-slug (encode-entities (logged-in-user-slug))))
            `("login" ,(format nil "/users/~A" user-slug) ,(plump:encode-entities username) :description "User page" :accesskey "u"
              :trailing-html ,(lambda (out-stream) (inbox-to-html out-stream user-slug))))
          `("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In" :accesskey "u" :nofollow t :override-uri "/login"))))

(defun sublevel-nav-to-html (out-stream options current &key default (base-uri (hunchentoot:request-uri*)) (param-name "show") (remove-params '("offset")) extra-class)
  (declare (type (or null string) extra-class))
  (format out-stream "<div class=\"sublevel-nav~@[ ~A~]\">" extra-class)
  (loop for item in options
        do (multiple-value-bind (param-value text) (if (atom item)
                                                       (values (string-downcase item) (string-capitalize item))
                                                       (values-list item))
             (let* ((selected (string-equal current param-value))
                    (class (if selected "sublevel-item selected" "sublevel-item")))
               (link-if-not out-stream selected (apply #'replace-query-params base-uri param-name (unless (string-equal param-value default) param-value)
                                                       (loop for x in remove-params nconc (list x nil)))
                            class text))))
  (format out-stream "</div>"))

(defun make-csrf-token (&optional (session-token (hunchentoot:cookie-in "session-token")) (nonce (ironclad:make-random-salt)))
  (if (typep session-token 'string) (setf session-token (base64:base64-string-to-usb8-array session-token)))
  (let ((csrf-token (concatenate '(vector (unsigned-byte 8)) nonce (ironclad:digest-sequence :sha256 (concatenate '(vector (unsigned-byte 8)) nonce session-token)))))
    (values (base64:usb8-array-to-base64-string csrf-token) csrf-token))) 

(defun check-csrf-token (csrf-token &optional (session-token (hunchentoot:cookie-in "session-token")))
  (let* ((session-token (base64:base64-string-to-usb8-array session-token))
	 (csrf-token (base64:base64-string-to-usb8-array csrf-token))
	 (correct-token (nth-value 1 (make-csrf-token session-token (subseq csrf-token 0 16)))))
    (assert (ironclad:constant-time-equal csrf-token correct-token) nil "CSRF check failed.")
    t)) 

(defun generate-css-link ()
  (labels ((gen-inner (theme os)
             (generate-versioned-link (format nil "/style~@[-~A~].~A.css" (if (and theme (> (length theme) 0)) theme) os))))
    (let* ((ua (hunchentoot:header-in* :user-agent))
           (theme (hunchentoot:cookie-in "theme"))
           (os (cond ((search "Windows" ua) "windows")
                     ((search "Mac OS" ua) "mac")
                     (t "linux"))))
      (handler-case (gen-inner theme os)
        (serious-condition () (gen-inner nil os))))))

(defun html-body (out-stream fn &key title description current-uri content-class robots)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
         (csrf-token (and session-token (make-csrf-token session-token))))
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head>")
    (format out-stream "<script>window.GW = { }; loggedInUserId=\"~A\"; loggedInUserDisplayName=\"~A\"; loggedInUserSlug=\"~A\"; ~@[GW.csrfToken=\"~A\"; ~]~A</script>~A"
            (or (logged-in-userid) "")
            (or (logged-in-username) "")
            (or (logged-in-user-slug) "")
            csrf-token
            (load-time-value (with-open-file (s "www/head.js") (uiop:slurp-stream-string s)) t)
            *extra-inline-scripts*)
    (format out-stream "~A<link rel=\"stylesheet\" href=\"~A\">~{<link rel=\"stylesheet\" href=\"~A\">~}<link rel=\"shortcut icon\" href=\"~A\">"
            *html-head*
            (generate-css-link)
            (generate-fonts-links)
            (generate-versioned-link "/favicon.ico"))
    (format out-stream "<script src=\"~A\" async></script>~A"
            (generate-versioned-link "/script.js")
            *extra-external-scripts*)
    (format out-stream "<title>~@[~A - ~]~A</title>~@[<meta name=\"description\" content=\"~A\">~]~@[<meta name=\"robots\" content=\"~A\">~]"
            (if title (encode-entities title))
            (site-title *current-site*)
            description
            robots)
    (format out-stream "</head>"))
  (unwind-protect
    (progn
      (format out-stream "<body><div id=\"content\"~@[ class=\"~A\"~]>"
              content-class)
      (nav-bar-to-html out-stream (or current-uri (replace-query-params (hunchentoot:request-uri*) "offset" nil "sort" nil)))
      (force-output out-stream)
      (funcall fn))
    (format out-stream "</div></body></html>")))

(defun replace-query-params (uri &rest params)
  (let* ((quri (quri:uri uri))
	 (old-params (quri:uri-query-params quri))
         (new-params (loop with out = old-params
                           for (param value) on params by #'cddr
                           do (if value
                                  (alexandria:if-let (old-cons (assoc param out :test #'equal))
                                                     (setf (cdr old-cons) value)
                                                     (setf out (nconc out (list (cons param value)))))
                                  (setf out (remove-if (lambda (x) (equal (car x) param)) out)))
                           finally (return out))))
    (if new-params 
      (setf (quri:uri-query-params quri) new-params)
      (setf (quri:uri-query quri) nil))
    (quri:render-uri quri)))

(defun pagination-nav-bars (&key offset total with-next (items-per-page (user-pref :items-per-page)))
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
            (format out-stream "<div id='top-nav-bar'>")
            (write-item first-uri "first" "First page" "\\")
            (write-item prev-uri "prev" "Previous page" "[")
            (format out-stream "<span class='page-number'><span class='page-number-label'>Page</span> ~A</span>" (+ 1 (/ (or offset 0) items-per-page)))
            (write-item next-uri "next" "Next page" "]")
            (write-item last-uri "last" "Last page" "/")
            (format out-stream "</div>")))
        (funcall fn)
	(nav-bar-outer out-stream nil (list :bottom-bar
					    (list-cond
					     (first-uri `("first" ,first-uri "Back to first"))
					     (prev-uri `("prev" ,prev-uri "Previous" :nofollow t))
					     (t `("top" "#top" "Back to top"))
					     (next-uri `("next" ,next-uri "Next" :nofollow t))
					     (last-uri `("last" ,last-uri "Last" :nofollow t)))))
	(format out-stream "<script>document.querySelectorAll('#bottom-bar').forEach(bb => { bb.classList.add('decorative'); });</script>")))))

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defun call-with-emit-page (out-stream fn &key title description current-uri content-class (return-code 200) robots (pagination (pagination-nav-bars)) top-nav)
  (declare (ignore return-code))
  (ignore-errors
    (log-conditions
      (html-body out-stream
                 (lambda ()
                   (when top-nav (funcall top-nav out-stream))
                   (funcall pagination out-stream fn))
                 :title title :description description :current-uri current-uri :content-class content-class :robots robots)
      (force-output out-stream))))

(defun set-cookie (key value &key (max-age (- (expt 2 31) 1)) (path "/"))
  (hunchentoot:set-cookie key :value value :path path :max-age max-age :secure (site-secure *current-site*)))

(defun set-default-headers (return-code)
  (let ((push-option (if (hunchentoot:cookie-in "push") '("nopush"))))
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
          (hunchentoot:return-code*) return-code
          (hunchentoot:header-out :link) (format nil "~:{<~A>;rel=preload;type=~A;as=~A~@{;~A~}~:^,~}"
                                                 `((,(generate-css-link) "text/css" "style" ,.push-option)
                                                   ,.(loop for link in (generate-fonts-links)
                                                           collect (list* link "text/css" "style" push-option))
                                                   (,(generate-versioned-link "/script.js") "text/javascript" "script" ,.push-option))))
    (unless push-option (set-cookie "push" "t" :max-age (* 4 60 60)))))

(defun user-pref (key)
  (or (cdr (assoc key *current-prefs*))
      (cdr (assoc key *default-prefs*))))

(defun set-user-pref (key value)
  (assert (boundp 'hunchentoot:*reply*))
  (setf *current-prefs* (remove-duplicates (acons key value *current-prefs*) :key #'car :from-end t))
  (set-cookie "prefs" (quri:url-encode (json:encode-json-to-string *current-prefs*))))

(defmacro with-response-stream ((out-stream) &body body) `(call-with-response-stream (lambda (,out-stream) ,.body)))

(defun call-with-response-stream (fn)
  (let ((*html-output* (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
    (funcall fn *html-output*)))

(defmacro emit-page ((out-stream &rest args &key (return-code 200) &allow-other-keys) &body body)
  (alexandria:once-only (return-code)
    `(progn
       (set-default-headers ,return-code)
       (with-response-stream (,out-stream)
         (call-with-emit-page ,out-stream
                              (lambda () ,@body)
                              ,@args)))))

(defun call-with-error-page (fn)
  (let* ((lw2-status
           (alexandria:if-let (status-string (hunchentoot:cookie-in "lw2-status"))
             (if (string= status-string "") nil
                 (let ((json:*identifier-name-to-key* #'json:safe-json-intern))
                   (json:decode-json-from-string status-string)))))
         (*current-prefs*
           (alexandria:if-let (prefs-string (hunchentoot:cookie-in "prefs"))
             (let ((json:*identifier-name-to-key* 'json:safe-json-intern))
               (ignore-errors (json:decode-json-from-string (quri:url-decode prefs-string)))))))
    (with-site-context ((let ((host (or (hunchentoot:header-in* :x-forwarded-host) (hunchentoot:header-in* :host))))
                          (or (find-site host)
                              (error "Unknown site: ~A" host))))
      (multiple-value-bind (*current-auth-token* *current-userid* *current-username*)
        (if *read-only-mode*
            (values)
            (alexandria:if-let
              (auth-token
                (alexandria:if-let
                  (at (hunchentoot:cookie-in "lw2-auth-token"))
                  (if (or (string= at "") (not lw2-status) (> (get-unix-time) (- (cdr (assoc :expires lw2-status)) (* 60 60 24))))
                      nil at)))
              (with-cache-readonly-transaction
                (values
                  auth-token
                  (cache-get "auth-token-to-userid" auth-token)
                  (cache-get "auth-token-to-username" auth-token)))))
        (let ((*current-user-slug* (and *current-userid* (get-user-slug *current-userid*))))
          (handler-case
            (log-conditions
              (funcall fn))
            (serious-condition (condition)
              (emit-page (out-stream :title "Error" :return-code (condition-http-return-code condition) :content-class "error-page")
                         (error-to-html out-stream condition)))))))))

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

(defun page-toolbar-to-html (out-stream &key title new-post new-conversation logout (rss t))
  (let ((liu (logged-in-userid)))
    (format out-stream "<div class=\"page-toolbar\">")
    (when logout
      (format out-stream "<form method=\"post\" action=\"/logout\"><button class=\"logout-button button\" name=\"logout\" value=\"~A\">Log out</button></form>"
              (make-csrf-token)))
    (when (and new-conversation liu)
      (multiple-value-bind (text to)
        (typecase new-conversation (string (values "Send private message" new-conversation)) (t "New conversation"))
        (format out-stream "<a class=\"new-private-message button\" href=\"/conversation~@[?to=~A~]\">~A</a>"
                to text)))
    (when (and new-post liu)
      (format out-stream "<a class=\"new-post button\" href=\"/edit-post~@[?section=~A~]\" accesskey=\"n\" title=\"Create new post [n]\">New post</a>"
              (typecase new-post (string new-post) (t nil))))
    (when (and title rss)
      (format out-stream "<a class=\"rss\" rel=\"alternate\" type=\"application/rss+xml\" title=\"~A RSS feed\" href=\"~A\">RSS</a>"
              title (replace-query-params (hunchentoot:request-uri*) "offset" nil "format" "rss")))
    (format out-stream "</div>")))

(defun view-items-index (items &key section title current-uri hide-title need-auth (pagination (pagination-nav-bars)) (top-nav (lambda (s) (page-toolbar-to-html s :title title))) (content-class "index-page"))
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
                     ("rss" 
                      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
                      (with-response-stream (out-stream)
                        (write-index-items-to-rss out-stream items :title title)))
                     (t
                       (emit-page (out-stream :title (if hide-title nil title) :description (site-description *current-site*) :content-class content-class
                                              :current-uri current-uri :robots (if (hunchentoot:get-parameter :offset) "noindex, nofollow")
                                              :pagination pagination :top-nav top-nav)
                                  (write-index-items-to-html out-stream items
                                                             :need-auth need-auth
                                                             :skip-section section)))))

(defun link-if-not (stream linkp url class text &key accesskey nofollow)
  (declare (dynamic-extent linkp url text))
  (if (not linkp)
      (format stream "<a href=\"~A\" class=\"~A\"~@[ accesskey=\"~A\"~]~:[~; rel=\"nofollow\"~]>~A</a>" url class accesskey nofollow text)
      (format stream "<span class=\"~A\">~A</span>" class text)))

(defun postprocess-markdown (markdown)
  (ppcre:regex-replace-all (concatenate 'string (ppcre:regex-replace-all "\\." (site-uri *current-site*) "\\.") "posts/([^/ ]{17})/([^/# ]*)(?:#comment-([^/ ]{17})|/comment/([^/ ]{17}))?")
                           markdown
                           (lambda (target-string start end match-start match-end reg-starts reg-ends)
                             (declare (ignore start end match-start match-end))
                             (labels ((reg (n) (if (and (> (length reg-starts) n) (aref reg-starts n))
                                                   (substring target-string (aref reg-starts n) (aref reg-ends n)))))
                               (format nil "https://www.lesswrong.com/posts/~A/~A~@[#~A~]" (reg 0) (reg 1) (or (reg 2) (reg 3)))))))

(defun post-or-get-parameter (name)
  (or (hunchentoot:post-parameter name) (hunchentoot:get-parameter name)))

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

(defmacro define-page (name path-specifier additional-vars &body body)
  (labels ((make-lambda (args)
             (loop for a in args
                   collect (if (atom a) a (first a))))
           (filter-plist (plist &rest args)
             (declare (dynamic-extent args))
             (map-plist (lambda (key val) (when (member key args) (list key val))) plist))
           (make-hunchentoot-lambda (args)
             (loop for x in args
                   collect (if (atom x) x
                               (cons (first x) (filter-plist (rest x) :request-type :real-name)))))
           (make-binding-form (additional-vars body &aux var-bindings additional-declarations additional-preamble)
             (loop for x in additional-vars
		when (not (eq x '*))
		do
		  (destructuring-bind (name &key member type default required request-type real-name) (if (atom x) (list x) x)
		    (declare (ignore request-type real-name))
		    (let* ((inner-form
			    (cond
			      (member
			       `(let ((sym (find-symbol (string-upcase ,name) ,(find-package '#:keyword))))
				  (if (member sym (quote ,member)) sym)))
			      ((and type (subtypep type 'integer))
			       `(if ,name (parse-integer ,name)))))
			   (inner-form
			    (if default
				`(or ,inner-form ,default)
				inner-form)))
		      (when required
			(push `(unless (and ,name (not (equal ,name ""))) (error "Missing required parameter: ~A" (quote ,name)))
			      additional-preamble))
		      (if member
			  (if type (error "Cannot specify both member and type.")
			      (push `(type (or null symbol) ,name) additional-declarations))
			  (if type
			      (push `(type (or null ,type) ,name) additional-declarations)
			      (push `(type (or null simple-string) ,name) additional-declarations)))
		      (when inner-form
			(push `(,name ,inner-form) var-bindings)))))
	     `(let ,var-bindings (declare ,@additional-declarations) ,@additional-preamble ,@body)))
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
     (let* ((rewritten-body
	     (if (eq (ignore-errors (caar body)) 'request-method)
		 (progn
		   (unless (= (length body) 1)
		     (error "REQUEST-METHOD must be the only form when it appears in DEFINE-PAGE."))
		   `((ecase (hunchentoot:request-method*)
		       ,.(loop for method-body in (cdar body)
			    collect (destructuring-bind (method args &body inner-body) method-body
				      (unless (eq method :get)
					(alexandria:with-gensyms (csrf-token)
					  (push `(,csrf-token :real-name "csrf-token" :required t) args)
					  (push `(check-csrf-token ,csrf-token) inner-body)))
				      (loop for a in args
					 do (push (append (if (atom a) (list a) (cons (first a) (filter-plist (rest a) :real-name))) (list :request-type method)) additional-vars))
				      `(,method ,(make-binding-form args inner-body)))))))
		 body)))
       `(hunchentoot:define-easy-handler (,name :uri ,path-specifier-form) ,(make-hunchentoot-lambda additional-vars)
	  (with-error-page
	      (block nil
		,(funcall path-bindings-wrapper
			  (make-binding-form (append specifier-vars additional-vars)
					     rewritten-body)))))))))

(define-component sort-widget (&key (sort-options '(:new :hot)) (pref :default-sort) (param-name "sort") (html-class "sort"))
  (:http-args '((sort :alias param-name :member sort-options)))
  (let ((sort-string (if sort (string-downcase sort))))
    (if sort-string
	(set-user-pref :default-sort sort-string))
    (renderer (out-stream)
      (sublevel-nav-to-html out-stream
			    sort-options
			    (user-pref pref)
			    :param-name param-name
			    :extra-class html-class))
    (or sort-string (user-pref pref))))

(define-page view-root "/" ((offset :type fixnum)
                            (limit :type fixnum))
  (component-value-bind ((sort-string sort-widget))
    (multiple-value-bind (posts total)
      (get-posts-index :offset offset :limit (or limit (user-pref :items-per-page)) :sort sort-string)
      (view-items-index posts
                        :section :frontpage :title "Frontpage posts" :hide-title t
                        :pagination (pagination-nav-bars :offset (or offset 0) :total total :with-next (not total))
                        :top-nav (lambda (out-stream)
                                   (page-toolbar-to-html out-stream
                                                         :title "Frontpage posts"
                                                         :new-post t)
                                   (funcall sort-widget out-stream))))))

(define-page view-index "/index" ((view :member (:all :new :frontpage :featured :meta :community :alignment-forum :questions) :default :all)
                                  before after
                                  (offset :type fixnum)
                                  (limit :type fixnum))
  (when (eq view :new) (redirect (replace-query-params (hunchentoot:request-uri*) "view" "all" "all" nil) :type :permanent) (return))
  (component-value-bind ((sort-string sort-widget))
    (multiple-value-bind (posts total)
      (get-posts-index :view (string-downcase view) :before before :after after :offset offset :limit (or limit (user-pref :items-per-page)) :sort sort-string)
      (let ((page-title (format nil "~@(~A posts~)" view)))
        (view-items-index posts
                          :section view :title page-title
                          :pagination (pagination-nav-bars :offset (or offset 0) :total total :with-next (not total))
                          :content-class (format nil "index-page ~(~A~)-index-page" view)
                          :top-nav (lambda (out-stream)
                                     (page-toolbar-to-html out-stream
                                                           :title page-title
                                                           :new-post (if (eq view :meta) "meta" t))
                                     (if (member view '(:all))
                                         (funcall sort-widget out-stream))))))))

(define-page view-post "/post" ((id :required t))
  (redirect (generate-post-link id) :type :permanent))

(define-page view-post-lw1-link (:function #'match-lw1-link) ()
  (redirect (convert-lw1-link (hunchentoot:request-uri*)) :type :permanent))

(define-page view-post-lw2-slug-link (:function #'match-lw2-slug-link) ()
  (redirect (convert-lw2-slug-link (hunchentoot:request-uri*)) :type :see-other))

(define-page view-post-lw2-sequence-link (:function #'match-lw2-sequence-link) ()
  (redirect (convert-lw2-sequence-link (hunchentoot:request-uri*)) :type :see-other))

(define-page view-feed "/feed" ()
  (redirect "/?format=rss" :type :permanent))

(define-page view-post-lw2-link (:function #'match-lw2-link post-id comment-id * comment-link-type) (need-auth chrono)
  (request-method
    (:get ()
     (let ((lw2-auth-token *current-auth-token*))
       (labels ((output-comments (out-stream id comments target)
                  (format out-stream "<div id=\"~A\" class=\"comments\">" id)
                  (with-error-html-block (out-stream)
                    (if target
                        (comment-thread-to-html out-stream
                                                (lambda ()
                                                  (comment-item-to-html
                                                    out-stream
                                                    target
                                                    :extra-html-fn (lambda (c-id)
                                                                     (let ((*comment-individual-link* nil))
                                                                       (comment-tree-to-html out-stream (make-comment-parent-hash comments) c-id))))))
                        (if comments
			    (if chrono
				(comment-chrono-to-html out-stream comments)
				(comment-tree-to-html out-stream (make-comment-parent-hash comments)))
			    <div class="comments-empty-message">(if (string= id "answers") "No answers." "No comments.")</div>)))
		  (format out-stream "</div>"))
		(output-comments-votes (out-stream)
                  (handler-case
                    (when lw2-auth-token
                      (format out-stream "<script>commentVotes=~A</script>"
                              (json:encode-json-to-string (get-post-comments-votes post-id lw2-auth-token))))
                    (t () nil)))
                (output-post-vote (out-stream)
                  (handler-case
                    (format out-stream "<script>postVote=~A</script>"
                            (json:encode-json-to-string (get-post-vote post-id lw2-auth-token)))
                    (t () nil))))
         (multiple-value-bind (post title condition)
           (handler-case (nth-value 0 (get-post-body post-id :auth-token (and need-auth lw2-auth-token)))
             (serious-condition (c) (values nil "Error" c))
             (:no-error (post) (values post (cdr (assoc :title post)) nil)))
           (if comment-id
	       (let* ((*comment-individual-link* t)
		      (comment-thread-type (if (string= comment-link-type "answer") :answer :comment))
		      (comments (case comment-thread-type
				  (:comment (get-post-comments post-id))
				  (:answer (get-post-answers post-id))))
		      (target-comment (find comment-id comments :key (lambda (c) (cdr (assoc :--id c))) :test #'string=))
		      (display-name (get-username (cdr (assoc :user-id target-comment))))
		      (verb-phrase (cond
				     ((and (eq comment-thread-type :answer)
					   (not (cdr (assoc :parent-comment-id target-comment))))
				      "answers")
				     (t "comments on"))))
		 (emit-page (out-stream :title (format nil "~A ~A ~A" display-name verb-phrase title)
					:content-class "individual-thread-page comment-thread-page")
			    (format out-stream "<h1 class=\"post-title\">~A ~A <a href=\"~A\">~A</a></h1>"
				    (encode-entities display-name)
				    verb-phrase
				    (generate-post-link post-id)
				    (clean-text-to-html title :hyphenation nil))
			    (output-comments out-stream "comments" comments target-comment)
			    (when lw2-auth-token
			      (force-output out-stream)
			      (output-comments-votes out-stream))))
	     (emit-page (out-stream :title title :content-class (format nil "post-page comment-thread-page~:[~; question-post-page~]" (cdr (assoc :question post))))
			(cond
			  (condition
                            (error-to-html out-stream condition))
                          (t
                           (post-body-to-html post)))
                        (when (and lw2-auth-token (equal (logged-in-userid) (cdr (assoc :user-id post))))
                          (format out-stream "<div class=\"post-controls\"><a class=\"edit-post-link button\" href=\"/edit-post?post-id=~A\" accesskey=\"e\" title=\"Edit post [e]\">Edit post</a></div>"
                                  (cdr (assoc :--id post))))
                        (force-output out-stream)
			(handler-case
			    (let* ((question (cdr (assoc :question post)))
				   (answers (when question
					      (get-post-answers post-id)))
				   (comments (get-post-comments post-id)))
			      (when question
				(output-comments out-stream "answers" answers nil))
			      (output-comments out-stream "comments" comments nil))
			  (serious-condition (c) (error-to-html out-stream c)))
			(when lw2-auth-token
                          (force-output out-stream)
                          (output-post-vote out-stream)
                          (output-comments-votes out-stream))))))))
    (:post (csrf-token text answer parent-answer-id parent-comment-id edit-comment-id retract-comment-id unretract-comment-id delete-comment-id)
     (let ((lw2-auth-token *current-auth-token*))
       (check-csrf-token csrf-token)
       (assert lw2-auth-token)
       (let ((question (cdr (assoc :question (get-post-body post-id :auth-token lw2-auth-token))))
	     (new-comment-id
	      (cond
		(text
		 (let ((comment-data
			(list-cond
			 (t :body (postprocess-markdown text))
			 (t :last-edited-as "markdown")
			 ((not edit-comment-id) :post-id post-id)
			 (parent-comment-id :parent-comment-id parent-comment-id)
			 (answer :answer t)
			 (parent-answer-id :parent-answer-id parent-answer-id))))
		   (if edit-comment-id
		       (prog1 edit-comment-id
			 (do-lw2-comment-edit lw2-auth-token edit-comment-id comment-data))
		       (do-lw2-comment lw2-auth-token comment-data))))
		(retract-comment-id
		 (do-lw2-comment-edit lw2-auth-token retract-comment-id '((:retracted . t))))
		(unretract-comment-id
		 (do-lw2-comment-edit lw2-auth-token unretract-comment-id '((:retracted . nil))))
		(delete-comment-id
		 (do-lw2-comment-edit lw2-auth-token delete-comment-id '((:deleted . t) (:deleted-public . t)
									 (:deleted-reason . "Comment deleted by its author.")))
		 nil))))
	 (ignore-errors
	   (get-post-comments post-id :force-revalidate t)
	   (when question
	     (get-post-answers post-id :force-revalidate t)))
	 (when text
	   (cache-put "comment-markdown-source" new-comment-id text)
	   (redirect (generate-post-link (match-lw2-link (hunchentoot:request-uri*)) new-comment-id))))))))

(defparameter *edit-post-template* (compile-template* "edit-post.html"))

(define-page view-edit-post "/edit-post" (title url section post-id link-post)
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
                                    :post-id post-id
                                    :section-list (loop for (name desc) in '(("all" "All") ("meta" "Meta") ("drafts" "Drafts"))
                                                        collect (alist :name name :desc desc :selected (string= name section)))
                                    :markdown-source (or (and post-id (cache-get "post-markdown-source" post-id)) (cdr (assoc :html-body post-body)) "")))))
    (:post (text question)
     (let ((lw2-auth-token *current-auth-token*)
           (url (if (string= url "") nil url)))
       (assert lw2-auth-token)
       (let* ((post-data
	       (list-cond
		(t :body (postprocess-markdown text))
		(t :title title)
		(t :last-edited-as "markdown")
		(t :url (if link-post url))
		(t :meta (string= section "meta"))
		(t :draft (string= section "drafts"))
		((not post-id) :question (and question t))))
	      (post-set (loop for item in post-data when (cdr item) collect item))
              (post-unset (loop for item in post-data when (not (cdr item)) collect (cons (car item) t))))
         (let* ((new-post-data
                  (if post-id
                      (do-lw2-post-edit lw2-auth-token post-id post-set post-unset)
                      (do-lw2-post lw2-auth-token post-set)))
                (new-post-id (cdr (assoc :--id new-post-data))))
           (assert new-post-id)
           (cache-put "post-markdown-source" new-post-id text)
           (ignore-errors (get-post-body post-id :force-revalidate t))
           (redirect (if (cdr (assoc "draft" post-data :test #'equal))
                         (concatenate 'string (generate-post-link new-post-data) "?need-auth=y")
                         (generate-post-link new-post-data)))))))))

(hunchentoot:define-easy-handler (view-karma-vote :uri "/karma-vote") ((csrf-token :request-type :post) (target :request-type :post) (target-type :request-type :post) (vote-type :request-type :post))
  (with-error-page
    (check-csrf-token csrf-token)
    (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token")))
      (multiple-value-bind (points vote-type) (do-lw2-vote lw2-auth-token target target-type vote-type)
        (json:encode-json-to-string (list (pretty-number points "point") vote-type))))))

(hunchentoot:define-easy-handler (view-check-notifications :uri "/check-notifications") ()
                                 (with-error-page
                                   (if *current-auth-token*
                                       (let ((notifications-status (check-notifications (logged-in-userid) *current-auth-token*)))
                                         (json:encode-json-to-string notifications-status)))))

(define-page view-recent-comments "/recentcomments" ((offset :type fixnum)
                                                     (limit :type fixnum))
  (let ((want-total (not (typep *current-backend* 'backend-lw2)))) ; jumping to last page causes LW2 to explode
    (multiple-value-bind (recent-comments total)
      (if (or offset limit (/= (user-pref :items-per-page) 20))
          (lw2-graphql-query (lw2-query-string :comment :list
                                               (remove nil (alist :view "postCommentsNew" :limit (or limit (user-pref :items-per-page)) :offset offset)
                                                       :key #'cdr)
                                               (comments-index-fields)
                                               :with-total want-total))
          (get-recent-comments :with-total want-total))
      (view-items-index recent-comments :title "Recent comments" :pagination (pagination-nav-bars :offset (or offset 0) :with-next (not want-total) :total (if want-total total))))))

(define-page view-user (:regex "^/users/(.*?)(?:$|\\?)|^/user" user-slug) (id
                                                                             (offset :type fixnum :default 0)
                                                                             (show :member (:all :posts :comments :drafts :conversations :inbox) :default :all)
                                                                             (sort :member (:top :new) :default :new))
             (let* ((auth-token (if (eq show :inbox) *current-auth-token*))
                    (user-query-terms (cond
                                        (user-slug (alist :slug user-slug))
                                        (id (alist :document-id id))))
                    (user-info
                      (let ((ui (lw2-graphql-query (lw2-query-string :user :single user-query-terms `(:--id :slug :display-name :karma ,@(if (eq show :inbox) '(:last-notifications-check))))
                                                   :auth-token auth-token)))
                        (if (cdr (assoc :--id ui))
                            ui
                            (error (make-condition 'lw2-user-not-found-error)))))
                    (user-id (cdr (assoc :--id user-info)))
                    (own-user-page (logged-in-userid user-id))
                    (comments-index-fields (remove :page-url (comments-index-fields))) ; page-url sometimes causes "Cannot read property '_id' of undefined" error
                    (display-name (if user-slug (cdr (assoc :display-name user-info)) user-id))
                    (show-text (if (not (eq show :all)) (string-capitalize show)))
                    (title (format nil "~A~@['s ~A~]" display-name show-text))
                    (sort-type (case sort (:top :score) (:new :date)))
                    (comments-base-terms (ecase sort-type (:score (load-time-value (alist :view "postCommentsTop"))) (:date (load-time-value (alist :view "allRecentComments"))))))
               (multiple-value-bind (items total)
                 (case show
                   (:posts
                     (get-user-posts user-id :offset offset :limit (+ 1 (user-pref :items-per-page)) :sort-type sort-type))
                   (:comments
                     (lw2-graphql-query (lw2-query-string :comment :list
                                                          (nconc (alist :offset offset :limit (+ 1 (user-pref :items-per-page)) :user-id user-id)
                                                                 comments-base-terms)
                                                          comments-index-fields)))
                   (:drafts
                     (get-user-posts user-id :drafts t :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                   (:conversations
                     (let ((conversations
                             (lw2-graphql-query (lw2-query-string :conversation :list
                                                                  (alist :view "userConversations" :limit (+ 1 (user-pref :items-per-page)) :offset offset :user-id user-id)
                                                                  '(:--id :created-at :title (:participants :display-name :slug) :----typename))
                                                :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                       (lw2-graphql-query-map
                         (lambda (c)
                           (lw2-query-string* :message :total (alist :view "messagesConversation" :conversation-id (cdr (assoc :--id c))) nil))
                         conversations
                         :postprocess (lambda (c result)
                                        (acons :messages-total result c))
                         :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                   (:inbox
                     (prog1
                       (let ((notifications (get-notifications :user-id user-id :offset offset :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                             (last-check (ignore-errors (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info))))))
                         (labels ((check-new (key obj)
                                    (if (ignore-errors (local-time:timestamp< last-check (local-time:parse-timestring (cdr (assoc key obj)))))
                                        (acons :highlight-new t obj)
                                        obj)))
                           (lw2-graphql-query-map
                             (lambda (n)
                               (alexandria:switch ((cdr (assoc :document-type n)) :test #'string=)
                                                  ("comment"
                                                   (lw2-query-string* :comment :single
                                                                      (alist :document-id (cdr (assoc :document-id n)))
                                                                      (comments-index-fields)))
                                                  ("post"
                                                   (lw2-query-string* :post :single (alist :document-id (cdr (assoc :document-id n)))
                                                                      (posts-index-fields)))
                                                  ("message"
                                                   (lw2-query-string* :message :single (alist :document-id (cdr (assoc :document-id n)))
                                                                      *messages-index-fields*))
                                                  (t
                                                    (values n t))))
                             notifications
                             :postprocess (lambda (n result)
                                            (if result
                                                (check-new
                                                  (alexandria:switch ((cdr (assoc :document-type n)) :test #'string=)
                                                                     ("comment" :posted-at)
                                                                     ("post" :posted-at)
                                                                     ("message" :created-at))
                                                  result)
                                                n))
                             :auth-token auth-token)))
		       (do-user-edit
			 (hunchentoot:cookie-in "lw2-auth-token")
			 user-id
			 (alist :last-notifications-check
				(local-time:format-timestring nil (local-time:now)
							      :format lw2.graphql:+graphql-timestamp-format+
							      :timezone local-time:+utc-zone+)))))
		   (t
		     (let ((user-posts (get-user-posts user-id :limit (+ 1 (user-pref :items-per-page) offset)))
			   (user-comments (lw2-graphql-query (lw2-query-string :comment :list (nconc (alist :limit (+ 1 (user-pref :items-per-page) offset) :user-id user-id) comments-base-terms) 
                                                                               comments-index-fields))))
                       (concatenate 'list user-posts user-comments))))
                 (let ((with-next (> (length items) (+ (if (eq show :all) offset 0) (user-pref :items-per-page))))
                       (interleave (if (eq show :all) (comment-post-interleave items :limit (user-pref :items-per-page) :offset (if (eq show :all) offset nil) :sort-by sort-type) (firstn items (user-pref :items-per-page))))) ; this destructively sorts items
                   (view-items-index interleave :title title
                                     :content-class (format nil "user-page~@[ ~A-user-page~]~:[~; own-user-page~]" show-text own-user-page)
                                     :current-uri (format nil "/users/~A" user-slug)
                                     :section :personal
                                     :pagination (pagination-nav-bars :offset offset :total total :with-next (if (not total) with-next))
                                     :need-auth (eq show :drafts) :section (if (eq show :drafts) "drafts" nil)
                                     :top-nav (lambda (out-stream)
                                                (page-toolbar-to-html out-stream
                                                                      :title title
                                                                      :rss (not (member show '(:drafts :conversations :inbox)))
                                                                      :new-post (if (eq show :drafts) "drafts" t)
                                                                      :new-conversation (if own-user-page t user-slug)
                                                                      :logout own-user-page)
                                                (format out-stream "<h1 class=\"page-main-heading\"~@[ ~A~]>~A</h1><div class=\"user-stats\">Karma: <span class=\"karma-total\">~A</span></div>"
                                                        (if (not own-user-page)
                                                            (if user-slug
                                                                (format nil "data-anti-kibitzer-redirect=\"/user?id=~A\"" (cdr (assoc :--id user-info)))
                                                                (format nil "data-kibitzer-redirect=\"/users/~A\"" (cdr (assoc :slug user-info)))))
                                                        (encode-entities display-name)
                                                        (if user-slug (pretty-number (or (cdr (assoc :karma user-info)) 0)) "##"))
                                                (sublevel-nav-to-html out-stream
                                                                      `(:all :posts :comments
                                                                        ,@(if own-user-page
                                                                              '(:drafts :conversations :inbox)))
                                                                      show
                                                                      :default :all)
                                                (when (member show '(:all :posts :comments))
                                                  (sublevel-nav-to-html out-stream
                                                                        '(:new :top)
                                                                        sort
                                                                        :default :new
                                                                        :param-name "sort"
                                                                        :extra-class "sort"))))))))

(defparameter *conversation-template* (compile-template* "conversation.html"))

(define-page view-conversation "/conversation" (id)
  (request-method
    (:get ()
     (let ((to (post-or-get-parameter "to")))
       (cond
         ((and id to) (error "This is an invalid URL."))
         (id
           (multiple-value-bind (conversation messages)
             (get-conversation-messages id (hunchentoot:cookie-in "lw2-auth-token"))
             (view-items-index (nreverse messages) :content-class "conversation-page" :need-auth t :title (encode-entities (postprocess-conversation-title (cdr (assoc :title conversation))))
                               :top-nav (lambda (out-stream) (render-template* *conversation-template* out-stream
                                                                               :conversation conversation :csrf-token (make-csrf-token))))))
         (t
          (emit-page (out-stream :title "New conversation" :content-class "conversation-page")
                     (render-template* *conversation-template* out-stream
                                       :to to
                                       :csrf-token (make-csrf-token)))))))
    (:post ((text :required t))
     (let* ((subject (post-or-get-parameter "subject"))
            (to (post-or-get-parameter "to"))
            (id (or id
                    (let ((participant-ids (list (logged-in-userid) (cdar (lw2-graphql-query (lw2-query-string :user :single (alist :slug to) '(:--id)))))))
                      (do-create-conversation (hunchentoot:cookie-in "lw2-auth-token") (alist :participant-ids participant-ids :title subject))))))
       (do-create-message (hunchentoot:cookie-in "lw2-auth-token") id text)
       (redirect (format nil "/conversation?id=~A" id))))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body
              (handler-case (markdown:parse (cdr (assoc :body item)))
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
                                         (csrf-token :request-type :post) (login-username :request-type :post) (login-password :request-type :post)
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
                    (alexandria:if-let (main-site-title (main-site-title *current-site*))
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
        (check-csrf-token csrf-token)
        (cond
          ((or (string= login-username "") (string= login-password "")) (emit-login-page :error-message "Please enter a username and password")) 
          (t (multiple-value-call #'finish-login login-username (do-login "username" login-username login-password))))) 
      (signup-username
        (check-csrf-token csrf-token)
        (cond
          ((not (every (lambda (x) (not (string= x ""))) (list signup-username signup-email signup-password signup-password2)))
           (emit-login-page :error-message "Please fill in all fields"))
          ((not (string= signup-password signup-password2))
           (emit-login-page :error-message "Passwords do not match"))
          (t (multiple-value-call #'finish-login signup-username (do-lw2-create-user signup-username signup-email signup-password)))))
      (t
       (emit-login-page))))) 

(define-page view-logout "/logout" ((logout :request-type :post))
  (check-csrf-token logout)
  (set-cookie "lw2-auth-token" "" :max-age 0)
  (redirect "/"))

(defparameter *reset-password-template* (compile-template* "reset-password.html"))

(define-page view-reset-password "/reset-password" ((csrf-token :request-type :post) (email :request-type :post) (reset-link :request-type :post) (password :request-type :post) (password2 :request-type :post))
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
        (check-csrf-token csrf-token)
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
                                       (check-csrf-token csrf-token)
                                       (multiple-value-bind (user-id auth-token error-message) (do-lw2-reset-password reset-token password)
                                         (declare (ignore user-id auth-token))
                                         (cond
                                           (error-message (emit-rpw-page :step 2 :message error-message :message-type "error"))
                                           (t
                                            (with-error-page (emit-page (out-stream :title "Reset password" :content-class "reset-password")
                                                                        (format out-stream "<h1>Password reset complete</h1><p>You can now <a href=\"/login\">log in</a> with your new password.</p>"))))))))))
      (t
       (emit-rpw-page)))))

(defun firstn (list n)
  (loop for x in list
	for i from 1 to n
	collect x)) 

(defparameter *earliest-post* (local-time:parse-timestring "2005-01-01")) 

(define-page view-archive (:regex "^/archive(?:/(\\d{4})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))"
                           (year :type (mod 10000))
                           (month :type (integer 1 12))
                           (day :type (integer 1 31)))
             ((offset :type fixnum :default 0))
  (local-time:with-decoded-timestamp (:day current-day :month current-month :year current-year) (local-time:now)
    (local-time:with-decoded-timestamp (:day earliest-day :month earliest-month :year earliest-year) *earliest-post*
      (labels ((url-elements (&rest url-elements)
                 (declare (dynamic-extent url-elements))
                 (format nil "/~{~A~^/~}" url-elements))
               (archive-nav (out-stream)
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
                 (format out-stream "</div>")))
        (multiple-value-bind (posts total)
          (lw2-graphql-query (lw2-query-string :post :list
                                               (alist :view (if day "new" "top") :limit 51 :offset offset
                                                      :after (if (and year (not day)) (format nil "~A-~A-~A" (or year earliest-year) (or month 1) (or day 1)))
                                                      :before (if year (format nil "~A-~A-~A" (or year current-year) (or month 12)
                                                                               (or day (local-time:days-in-month (or month 12) (or year current-year))))))
                                               (posts-index-fields)))
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
                                                                                      (loop for theme in '(nil "dark" "grey" "ultramodern" "zero" "brutalist" "rts")
                                                                                            collect (defres (format nil "/style~@[-~A~].~A.css" theme system) "text/css")))
                                                                                    (loop for (uri content-type) in
                                                                                      '(("/script.js" "text/javascript")
                                                                                        ("/favicon.ico" "image/x-icon"))
                                                                                      collect (defres uri content-type))))
                                                                   (when file
                                                                     (when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
                                                                       (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
                                                                     (hunchentoot:handle-static-file file content-type)
                                                                     t))))
                                 nil)
