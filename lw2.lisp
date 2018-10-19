(uiop:define-package #:lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:djula #:lw2-viewer.config #:lw2.utils #:lw2.lmdb #:lw2.backend #:lw2.links #:lw2.clean-html #:lw2.login))

(in-package #:lw2-viewer) 

(add-template-directory (asdf:system-relative-pathname "lw2-viewer" "templates/"))

(defvar *current-auth-token*) 

(defvar *read-only-mode* nil)
(defvar *read-only-default-message* "Due to a system outage, you cannot log in or post at this time.")

(defparameter *default-prefs* (alist :items-per-page 20 :default-sort "new"))
(defvar *current-prefs* nil)

(defun logged-in-userid (&optional is-userid)
  (if *read-only-mode* nil
      (let ((current-userid (and *current-auth-token* (cache-get "auth-token-to-userid" *current-auth-token*))))
        (if is-userid
            (string= current-userid is-userid)
            current-userid)))) 
(defun logged-in-username ()
  (and (not *read-only-mode*) *current-auth-token* (cache-get "auth-token-to-username" *current-auth-token*))) 

(defun pretty-time (timestring &key format)
  (let ((time (local-time:parse-timestring timestring)))
  (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	  (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-number (number &optional object)
  (let ((str (coerce (format nil "~:D~@[<span> ~A~P</span>~]" number object number) '(vector character))))
    (if (eq (aref str 0) #\-)
      (setf (aref str 0) #\MINUS_SIGN))
    str))

(defun encode-entities (text)
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (plump:encode-entities (format nil "~A" text))))

(defun generate-post-auth-link (post &optional comment-id absolute need-auth)
  (if need-auth
      (concatenate 'string (generate-post-link post comment-id absolute) "?need-auth=y")
      (generate-post-link post comment-id absolute)))

(defun clean-lw-link (url)
  (when url
    (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com")))

(defmacro alist-bind (bindings alist &body body)
  (alexandria:once-only (alist)
    (let ((inner-bindings (loop for x in bindings collect
                                (destructuring-bind (bind &optional type key) (if (consp x) x (list x))
                                  (list (gensym (string bind)) (gensym (string bind)) (gensym (string bind)) bind (or type t) (or key (intern (string bind) '#:keyword)))))))
      (macrolet ((inner-loop (&body body)
                   `(loop for (fn-gensym cons-gensym value-gensym bind type key) in inner-bindings collect
                          (progn fn-gensym cons-gensym value-gensym bind type key ,@body))))
        `(let* (,@(inner-loop `(,cons-gensym (assoc ,key ,alist)))
                ,@(inner-loop `(,value-gensym (cdr ,cons-gensym))))
             (declare ,@(inner-loop `(type ,type ,value-gensym)))
             (flet (,@(inner-loop `(,fn-gensym () ,value-gensym))
                    ,@(inner-loop `((setf ,fn-gensym) (new) (setf (cdr ,cons-gensym) new ,value-gensym new))))
               (declare (inline ,@(inner-loop fn-gensym)))
               (symbol-macrolet ,(inner-loop `(,bind (,fn-gensym)))
                 ,@body)))))))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun post-section-to-html (out-stream post &key skip-section)
  (alist-bind ((user-id string)
               (frontpage-date (or null string))
               (curated-date (or null string))
               (meta boolean)
               (af boolean)
               (draft boolean))
    post
    (format out-stream "~1{<a class=\"post-section ~A\" title=\"~A\"~1@{ href=\"~A\"~}></a>~}"
            (cond (af (if (eq skip-section :alignment-forum) nil (list "alignment-forum" "View Alignment Forum posts" "/index?view=alignment-forum")))
                  ; show alignment forum even if skip-section is t
                  ((eq skip-section t) nil)
                  (draft nil)
                  (curated-date (if (eq skip-section :featured) nil (list "featured" "View Featured posts" "/index?view=featured")))
                  (frontpage-date (if (eq skip-section :frontpage) nil (list "frontpage" "View Frontpage posts" "/")))
                  (meta (if (eq skip-section :meta) nil (list "meta" "View Meta posts" "/index?view=meta")))
                  (t (if (eq skip-section :personal) nil (list "personal" (format nil "View posts by ~A" (get-username user-id)) (format nil "/users/~A?show=posts" (get-user-slug user-id)))))))))

(defun post-headline-to-html (out-stream post &key skip-section need-auth)
  (alist-bind ((title string)
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
               (vote-count (or null fixnum))
               (draft boolean))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<h1 class=\"listing~:[~; link-post-listing~]\">~@[<a href=\"~A\">&#xf0c1;</a>~]<a href=\"~A\">~A</a></h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\" data-userid=\"~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\"><span class=\"karma-value\" title=\"~A\">~A</span></div><a class=\"comment-count\" href=\"~A#comments\">~A</a>~:[~*~;~:*<span class=\"read-time\" title=\"~:D word~:P\">~:D<span> min read</span></span>~]~@[<a class=\"lw2-link\" href=\"~A\">LW<span> link</span></a>~]"
              url
              (if url (encode-entities (string-trim " " url)))
              (generate-post-auth-link post nil nil need-auth)
              (clean-text-to-html title)
              (encode-entities (get-user-slug user-id))
              (encode-entities user-id)
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              (votes-to-tooltip vote-count)
              (pretty-number base-score "point")
              (generate-post-link post)
              (pretty-number (or comment-count 0) "comment")
              word-count
              (and word-count (max 1 (round word-count 300)))
              (clean-lw-link page-url)))
    (post-section-to-html out-stream post :skip-section skip-section)
    (if url (format out-stream "<div class=\"link-post-domain\">(~A)</div>" (encode-entities (puri:uri-host (puri:parse-uri (string-trim " " url))))))
    (format out-stream "</div>")))

(defun post-body-to-html (out-stream post)
  (alist-bind ((post-id string :--id)
               (title string)
               (user-id string)
               (url (or null string))
               (posted-at string)
               (base-score fixnum)
               (comment-count (or null fixnum))
               (page-url (or null string))
               (frontpage-date (or null string))
               (curated-date (or null string))
               (meta boolean)
               (draft boolean)
               (af boolean)
               (vote-count (or null fixnum))
               (html-body (or null string)))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<div class=\"post~:[~; link-post~]\"><h1>~A</h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\" data-userid=\"~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\" data-post-id=\"~A\"><span class=\"karma-value\" title=\"~A\">~A</span></div><a class=\"comment-count\" href=\"#comments\">~A</a>~@[<a class=\"lw2-link\" href=\"~A\">LW<span> link</span></a>~]"
              url
              (clean-text-to-html title)
              (encode-entities (get-user-slug user-id))
              (encode-entities user-id)
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              post-id
              (votes-to-tooltip vote-count)
              (pretty-number base-score "point")
              (pretty-number (or comment-count 0) "comment")
              (clean-lw-link page-url)))
    (post-section-to-html out-stream post)
    (format out-stream "</div><div class=\"post-body\">")
    (if url (format out-stream "<p><a href=\"~A\" class=\"link-post-link\">Link post</a></p>" (encode-entities (string-trim " " url))))
    (write-sequence (clean-html* (or html-body "") :with-toc t :post-id post-id) out-stream)
    (format out-stream "</div></div>")))

(defparameter *comment-individual-link* nil)

(defun comment-to-html (out-stream comment &key with-post-title)
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
               (html-body string))
    comment
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<div class=\"comment~{ ~A~}\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\" data-userid=\"~A\">~A</a> <a class=\"date\" href=\"~A\" data-js-date=\"~A\">~A</a><div class=\"karma\"><span class=\"karma-value\" title=\"~A\">~A</span></div><a class=\"permalink\" href=\"~A/comment/~A\" title=\"Permalink\"></a>~@[<a class=\"lw2-link\" href=\"~A\" title=\"LW link\"></a>~]"
              (let ((l nil))
                (if (and (logged-in-userid user-id) (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000))) (push "just-posted-comment" l))
                (if highlight-new (push "comment-item-highlight" l))
                l)
              (encode-entities (get-user-slug user-id))
              (encode-entities user-id)
              (encode-entities (get-username user-id))
              (generate-post-link post-id comment-id)
              js-time
              pretty-time
              (votes-to-tooltip vote-count)
              (pretty-number base-score "point")
              (generate-post-link post-id)
              comment-id
              (clean-lw-link page-url)))
    (if with-post-title
        (format out-stream "<div class=\"comment-post-title\">~1{<span class=\"comment-in-reply-to\">in reply to: <a href=\"/users/~A\" data-userid=\"~A\">~A</a>’s <a href=\"~A\">comment</a></span> ~}<span class=\"comment-post-title2\">on: <a href=\"~A\">~A</a></span></div>"
                (alexandria:if-let (parent-comment parent-comment)
                                   (list (encode-entities (get-user-slug (cdr (assoc :user-id parent-comment))))
                                         (encode-entities (cdr (assoc :user-id parent-comment)))
                                         (encode-entities (get-username (cdr (assoc :user-id parent-comment))))
                                         (generate-post-link (cdr (assoc :post-id parent-comment)) (cdr (assoc :--id parent-comment)))))
                (generate-post-link post-id)
                (clean-text-to-html (get-post-title post-id)))
        (progn
          (when parent-comment-id
            (if *comment-individual-link*
                (format out-stream "<a class=\"comment-parent-link\" href=\"~A\" title=\"Parent\"></a>" parent-comment-id)
                (format out-stream "<a class=\"comment-parent-link\" href=\"#comment-~A\">Parent</a>" parent-comment-id)))
          (format out-stream "~@[<div class=\"comment-child-links\">Replies: ~:{<a href=\"#comment-~A\">&gt;~A</a>~}</div>~]~:[~;<div class=\"comment-minimize-button\" data-child-count=\"~A\"></div>~]"
                  (map 'list (lambda (c) (list (cdr (assoc :comment-id c)) (get-username (cdr (assoc :user-id c))))) children)
                  (not parent-comment-id)
                  child-count)))
    (format out-stream "</div><div class=\"comment-body\"~@[ data-markdown-source=\"~A\"~]>"
            (if (logged-in-userid user-id)
                (encode-entities
                  (or (cache-get "comment-markdown-source" comment-id)
                      html-body))))
    (write-sequence (clean-html* html-body) out-stream)
    (format out-stream "</div></div>")))

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
  `(handler-case
     (progn ,@body)
     (serious-condition (c) (error-to-html ,out-stream c))))

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
             (post-headline-to-html out-stream x :need-auth need-auth :skip-section skip-section))
            (t
             (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item\" id=\"comment-~A\">" (cdr (assoc :--id x)))
             (unwind-protect
               (comment-to-html out-stream x :with-post-title t)
               (format out-stream "</li></ul>"))))))
      (format out-stream "<div class=\"listing-message\">~A</div>" empty-message)))

(defun write-index-items-to-rss (out-stream items &key title need-auth)
  (let ((full-title (format nil "~@[~A - ~]LessWrong 2 viewer" title)))
    (xml-emitter:with-rss2 (out-stream :encoding "UTF-8")
      (xml-emitter:rss-channel-header full-title *site-uri* :description full-title)
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

(defparameter *fonts-stylesheet-uri* "https://fonts.greaterwrong.com/?fonts=Charter,Concourse,a_Avante,Whitney,MundoSans,SourceSansPro,Raleway,ProximaNova,AnonymousPro,InputSans,InputSansNarrow,InputSansCondensed,GaramondPremierPro,ProximaNova,TradeGothic,NewsGothicBT,Caecilia,SourceSerifPro,SourceCodePro,Inconsolata,BitmapFonts,FontAwesomeGW")
(defparameter *fonts-stylesheet-uri* "https://fonts.greaterwrong.com/?fonts=*")

(defvar *fonts-redirect-data* nil)
(sb-ext:defglobal *fonts-redirect-lock* (make-mutex))
(sb-ext:defglobal *fonts-redirect-thread* nil)

(defun generate-fonts-link ()
  (let ((current-time (get-unix-time)))
    (labels ((get-redirect (uri)
               (multiple-value-bind (body status headers uri)
                 (drakma:http-request uri :method :head :close t :redirect nil :additional-headers (alist :referer *site-uri* :accept "text/css,*/*;q=0.1"))
                 (declare (ignore body uri))
                 (let ((location (cdr (assoc :location headers))))
                   (if (and (typep status 'integer) (< 300 status 400) location)
                       location
                       nil))))
             (update-redirect ()
               (handler-case
                 (let* ((new-redirect (get-redirect *fonts-stylesheet-uri*))
                        (new-redirect (if new-redirect (quri:render-uri (quri:merge-uris (quri:uri new-redirect) (quri:uri *fonts-stylesheet-uri*))) *fonts-stylesheet-uri*)))
                   (with-mutex (*fonts-redirect-lock*) (setf *fonts-redirect-data* (list *fonts-stylesheet-uri* new-redirect current-time)
                                                             *fonts-redirect-thread* nil))
                   new-redirect)
                 (serious-condition () *fonts-stylesheet-uri*))))
      (destructuring-bind (&optional base-uri redirect-uri timestamp) (with-mutex (*fonts-redirect-lock*) *fonts-redirect-data*)
        (if (and (eq base-uri *fonts-stylesheet-uri*) timestamp)
          (progn
            (if (>= current-time (+ timestamp 60))
                (with-mutex (*fonts-redirect-lock*) (or *fonts-redirect-thread* (make-thread #'update-redirect :name "fonts redirect update"))))
            (or redirect-uri *fonts-stylesheet-uri*))
          (update-redirect))))))

(defparameter *html-head*
  (format nil
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta name=\"HandheldFriendly\" content=\"True\" />"))

(defparameter *extra-external-scripts* "")
(defparameter *extra-inline-scripts* "")

(defun generate-versioned-link (file)
  (format nil "~A?v=~A" file (sb-posix:stat-mtime (sb-posix:stat (format nil "www~A" file))))) 

(defun search-bar-to-html ()
  (declare (special *current-search-query*))
  (let ((query (and (boundp '*current-search-query*) (hunchentoot:escape-for-html *current-search-query*))))
    (format nil "<form action=\"/search\" class=\"nav-inner\"><input name=\"q\" type=\"search\" ~@[value=\"~A\"~] autocomplete=\"off\" accesskey=\"s\" title=\"Search [s]&#10;Tip: Paste a LessWrong URL here to jump to that page.\"><button>Search</button></form>" query)))  

(defun inbox-to-html (user-slug &optional new-messages)
  (let* ((target-uri (format nil "/users/~A?show=inbox" user-slug))
         (as-link (string= (hunchentoot:request-uri*) target-uri)))
    (multiple-value-bind (nm-class nm-text)
      (if new-messages (values "new-messages" "New messages") (values "no-messages" "Inbox"))
      (format nil "<~:[a href=\"~A\"~;span~*~] id=\"inbox-indicator\" class=\"~A\" accesskey=\"o\" title=\"~A~:[ [o]~;~]\">~A</a>"
              as-link target-uri nm-class nm-text as-link nm-text))))

(defparameter *primary-nav* '(("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
			      ("featured" "/index?view=featured" "Featured" :description "Latest featured posts" :accesskey "f")
			      ("all" "/index?view=new" "All" :description "Latest posts from all sections" :accesskey "a")
			      ("meta" "/index?view=meta" "Meta" :description "Latest meta posts" :accesskey "m")
			      ("recent-comments" "/recentcomments" "<span>Recent </span>Comments" :description "Latest comments" :accesskey "c"))) 

(defparameter *secondary-nav* nil)

(defun nav-bar-inner (items &optional current-uri)
  (let ((active-bar nil)) 
    (values
      (format nil "~{~A~}"
	      (maplist (lambda (items)
			 (let ((item (first items))) 
			   (destructuring-bind (id uri name &key description html accesskey nofollow trailing-html) item
			     (if (string= uri current-uri)
			       (progn (setf active-bar t)
				      (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-current\" ~@[title=\"~A\"~]>~:[<span class=\"nav-inner\">~A</span>~@[~A~]~;~:*~A~]</span>"
					      id description (and html (funcall html)) name trailing-html))
			       (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-inactive~:[~; nav-item-last-before-current~]\" ~@[title=\"~A\"~]>~:[<a href=\"~A\" class=\"nav-inner\"~@[ accesskey=\"~A\"~]~:[~; rel=\"nofollow\"~]>~A</a>~@[~A~]~;~:*~A~]</span>"
				       id (string= (nth 1 (cadr items)) current-uri) (if accesskey (format nil "~A [~A]" (or description name) accesskey) description) (and html (funcall html)) uri accesskey nofollow name
				       trailing-html)))))
		       items))
      active-bar)))

(defun nav-bar-outer (id class html)
  (format nil "<div id=\"~A\" class=\"nav-bar~@[ ~A~]\">~A</div>" id class html))

(defun nav-bar-to-html (&optional current-uri)
  (let ((primary-bar "primary-bar")
	(secondary-bar "secondary-bar")) 
    (let ((primary-html (nav-bar-inner *primary-nav* current-uri)))
      (multiple-value-bind (secondary-html secondary-active)
	(nav-bar-inner *secondary-nav* current-uri)
	(if secondary-active
	  (format nil "~A~A" (nav-bar-outer primary-bar "inactive-bar" primary-html) (nav-bar-outer secondary-bar "active-bar" secondary-html))
	  (format nil "~A~A" (nav-bar-outer secondary-bar "inactive-bar" secondary-html) (nav-bar-outer primary-bar "active-bar" primary-html))))))) 

(defun user-nav-bar (&optional current-uri)
  (let* ((username (logged-in-username))
         (*secondary-nav* `(("archive" "/archive" "Archive" :accesskey "r")
                            ("about" "/about" "About" :accesskey "t")
                            ("search" "/search" "Search" :html ,#'search-bar-to-html)
                            ,(if *read-only-mode*
                                 `("login" "/login" "Read Only Mode" :html ,(lambda () (format nil "<span class=\"nav-inner\" title=\"~A\">[Read Only Mode]</span>"
                                                                                               (typecase *read-only-mode*
                                                                                                 (string *read-only-mode*)
                                                                                                 (t *read-only-default-message*)))))
                                 (if username
                                     (let ((user-slug (encode-entities (get-user-slug (logged-in-userid)))))
                                       `("login" ,(format nil "/users/~A" user-slug) ,(plump:encode-entities username) :description "User page" :accesskey "u"
                                         :trailing-html ,(inbox-to-html user-slug)))
                                     `("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In" :accesskey "u" :nofollow t))))))
    (nav-bar-to-html current-uri)))

(defun sublevel-nav-to-html (out-stream options current &key (base-uri (hunchentoot:request-uri*)) (param-name "show") (remove-params '("offset")) extra-class)
  (declare (type (or null string) extra-class))
  (format out-stream "<div class=\"sublevel-nav~@[ ~A~]\">" extra-class)
  (loop for (param-value text) in options
        do (link-if-not out-stream (string= current param-value) (apply #'replace-query-params (list* base-uri param-name param-value (loop for x in remove-params nconc (list x nil))))
                        "sublevel-item" text))
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

(defun begin-html (out-stream &key title description current-uri content-class robots)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
         (csrf-token (and session-token (make-csrf-token session-token)))) 
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head>")
    (format out-stream "<style id='width-adjust'></style><script>loggedInUserId=\"~A\"; ~@[var csrfToken=\"~A\"; ~]~A</script>~A"
            (or (logged-in-userid) "")
            csrf-token
            (load-time-value (with-open-file (s "www/head.js") (uiop:slurp-stream-string s)) t)
            *extra-inline-scripts*)
    (format out-stream "~A<link rel=\"stylesheet\" href=\"~A\"><link rel=\"stylesheet\" href=\"~A\"><link rel=\"stylesheet\" href=\"~A\"><link rel=\"shortcut icon\" href=\"~A\">"
            *html-head*
            (generate-css-link)
            (generate-versioned-link "/theme_tweaker.css")
            (generate-fonts-link)
            (generate-versioned-link "/favicon.ico"))
    (format out-stream "<script src=\"~A\" async></script>~A"
            (generate-versioned-link "/script.js")
            *extra-external-scripts*)
    (format out-stream "<title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~@[<meta name=\"robots\" content=\"~A\">~]"
            (if title (encode-entities title))
            description
            robots)
    (format out-stream "</head><body><div id=\"content\"~@[ class=\"~A\"~]>~A"
            content-class
            (user-nav-bar (or current-uri (replace-query-params (hunchentoot:request-uri*) "offset" nil "sort" nil)))))
  (force-output out-stream))

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

(defun pagination-nav-bars (&key with-offset with-next items-per-page)
  (let* ((next (if (and with-offset with-next) (+ with-offset items-per-page)))
         (prev (if (and with-offset (>= with-offset items-per-page)) (- with-offset items-per-page)))
         (request-uri (hunchentoot:request-uri*))
         (first-uri (if (and prev (> prev 0)) (replace-query-params request-uri "offset" nil)))
         (prev-uri (if prev (replace-query-params request-uri "offset" (if (= prev 0) nil prev))))
         (next-uri (if next (replace-query-params request-uri "offset" next))))
    (values
      (if (or next prev)
          (with-output-to-string (out-stream)
            (labels ((write-item (uri class title accesskey)
                       (format out-stream "<a href=\"~A\" class=\"button nav-item-~A~:[ disabled~;~]\" title=\"~A (accesskey: '~A')\" accesskey=\"~A\"></a>"
                               (or uri "#") class uri title accesskey accesskey)))
              (format out-stream "<div id='top-nav-bar'>")
              (write-item first-uri "first" "First page" "\\")
              (write-item prev-uri "prev" "Previous page" "[")
              (format out-stream "<span class='page-number'><span class='page-number-label'>Page</span> ~A</span>" (+ 1 (/ (or with-offset 0) items-per-page)))
              (write-item next-uri "next" "Next page" "]")
              (write-item next-uri "last" "Last page" "")
              (format out-stream "</div>")))
          "")
      (with-output-to-string (out-stream)
        (write-string
          (nav-bar-outer "bottom-bar" nil (nav-bar-inner
                                            `(,@(if first-uri `(("first" ,first-uri "Back to first")))
                                               ,@(if prev-uri `(("prev" ,prev-uri "Previous" :nofollow t)))
                                               ("top" "#top" "Back to top")
                                               ,@(if next-uri `(("next" ,next-uri "Next" :nofollow t))))))
          out-stream)
        (format out-stream "<script>document.querySelectorAll('#bottom-bar').forEach(function (bb) { bb.classList.add('decorative'); });</script>")))))

(defun end-html (out-stream &key items-per-page bottom-bar-html)
  (if items-per-page (format out-stream "<script>var itemsPerPage=~A</script>" items-per-page))
  (write-string bottom-bar-html out-stream)
  (format out-stream "</div></body></html>"))

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defun call-with-emit-page (out-stream fn &key title description current-uri content-class (return-code 200) (items-per-page (user-pref :items-per-page)) with-offset with-next robots)
  (declare (ignore return-code))
  (ignore-errors
    (log-conditions
      (multiple-value-bind (top-bar-html bottom-bar-html) (pagination-nav-bars :with-offset with-offset :with-next with-next :items-per-page items-per-page)
        (begin-html out-stream :title title :description description :current-uri current-uri :content-class content-class :robots robots)
        (funcall fn (lambda () (write-string top-bar-html out-stream)))
        (end-html out-stream
                  :items-per-page (if with-offset items-per-page)
                  :bottom-bar-html bottom-bar-html))
      (force-output out-stream))))

(defun set-default-headers (return-code)
  (let ((push-option (if (hunchentoot:cookie-in "push") '("nopush"))))
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
          (hunchentoot:return-code*) return-code
          (hunchentoot:header-out :link) (format nil "~:{<~A>;rel=preload;type=~A;as=~A~@{;~A~}~:^,~}"
                                                 `((,(generate-css-link) "text/css" "style" ,.push-option)
                                                   (,(generate-versioned-link "/theme_tweaker.css") "text/css" "style" ,.push-option)
                                                   (,(generate-fonts-link) "text/css" "style" ,.push-option)
                                                   (,(generate-versioned-link "/script.js") "text/javascript" "script" ,.push-option)
                                                   ("//fonts.greaterwrong.com/font_files/FontAwesomeGW/fa-solid-900.ttf?v=1" "font/ttf" "font" "crossorigin")
                                                   ("//fonts.greaterwrong.com/font_files/FontAwesomeGW/fa-regular-400.ttf?v=1" "font/ttf" "font" "crossorigin")
                                                   ("//fonts.greaterwrong.com/font_files/FontAwesomeGW/fa-light-300.ttf?v=1" "font/ttf" "font" "crossorigin")
                                                   ("//fonts.greaterwrong.com/font_files/BitmapFonts/MSSansSerif.ttf" "font/ttf" "font" "crossorigin"))))
    (unless push-option (hunchentoot:set-cookie "push" :max-age (* 4 60 60) :secure *secure-cookies* :value "t"))))

(defun user-pref (key)
  (or (cdr (assoc key *current-prefs*))
      (cdr (assoc key *default-prefs*))))

(defun set-user-pref (key value)
  (assert (boundp 'hunchentoot:*reply*))
  (setf *current-prefs* (remove-duplicates (acons key value *current-prefs*) :key #'car :from-end t))
  (hunchentoot:set-cookie "prefs" :max-age (- (expt 2 31) 1) :secure *secure-cookies* :value (quri:url-encode (json:encode-json-to-string *current-prefs*))))

(defmacro emit-page ((out-stream &rest args &key (return-code 200) (top-nav (gensym) top-nav-supplied) &allow-other-keys) &body body)
  (alexandria:once-only (return-code)
    (alexandria:with-gensyms (fn)
      (let ((args2 (copy-seq args)))
        (remf args2 :top-nav)
        `(progn
           (set-default-headers ,return-code)
           (let* ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8))
                  (,fn (lambda (,top-nav) ,@(if (not top-nav-supplied) `((declare (ignore ,top-nav)))) ,@body)))
             (call-with-emit-page ,out-stream ,fn ,@args2)))))))

(defun call-with-error-page (fn)
  (let* ((lw2-status
           (alexandria:if-let (status-string (hunchentoot:cookie-in "lw2-status"))
             (if (string= status-string "") nil
                 (let ((json:*identifier-name-to-key* #'json:safe-json-intern))
                   (json:decode-json-from-string status-string)))))
         (*current-auth-token*
           (alexandria:if-let (at (hunchentoot:cookie-in "lw2-auth-token"))
             (if (or (string= at "") (not lw2-status) (> (get-unix-time) (- (cdr (assoc :expires lw2-status)) (* 60 60 24))))
                 nil at)))
         (*current-prefs*
           (alexandria:if-let (prefs-string (hunchentoot:cookie-in "prefs"))
             (let ((json:*identifier-name-to-key* 'json:safe-json-intern))
               (ignore-errors (json:decode-json-from-string (quri:url-decode prefs-string)))))))
    (handler-case
      (log-conditions 
        (funcall fn))
      (serious-condition (condition)
                         (emit-page (out-stream :title "Error" :return-code (condition-http-return-code condition) :content-class "error-page")
                                    (error-to-html out-stream condition))))))

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

(defun view-items-index (items &key section with-offset (with-next t) title current-uri hide-title need-auth (extra-html (lambda (s) (page-toolbar-to-html s :title title))) (content-class "index-page"))
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
                     ("rss" 
                      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
                      (let ((out-stream (hunchentoot:send-headers)))
                        (write-index-items-to-rss (make-flexi-stream out-stream :external-format :utf-8) items :title title)))
                     (t
                       (emit-page (out-stream :title (if hide-title nil title) :description "A faster way to browse LessWrong 2.0" :content-class content-class :with-offset with-offset :with-next with-next
                                              :current-uri current-uri :robots (if (and with-offset (> with-offset 0)) "noindex, nofollow") :top-nav top-nav)
                                  (typecase extra-html
                                    (function (funcall extra-html out-stream))
                                    (t (format out-stream "~@[~A~]" extra-html)))
                                  (funcall top-nav)
                                  (write-index-items-to-html out-stream items :need-auth need-auth
                                                             :skip-section section)))))

(defun link-if-not (stream linkp url class text)
  (declare (dynamic-extent linkp url text))
  (if (not linkp)
      (format stream "<a href=\"~A\" class=\"~A\">~A</a>" url class text)
      (format stream "<span class=\"~A\">~A</span>" class text)))

(defun postprocess-markdown (markdown)
  (ppcre:regex-replace-all (load-time-value (concatenate 'string (ppcre:regex-replace-all "\\." *site-uri* "\\.") "posts/([^/ ]{17})/([^/# ]*)(?:#comment-([^/ ]{17})|/comment/([^/ ]{17}))?"))
                           markdown
                           (lambda (target-string start end match-start match-end reg-starts reg-ends)
                             (declare (ignore start end match-start match-end))
                             (labels ((reg (n) (if (and (> (length reg-starts) n) (aref reg-starts n))
                                                   (substring target-string (aref reg-starts n) (aref reg-ends n)))))
                               (format nil "https://www.lesswrong.com/posts/~A/~A~@[#~A~]" (reg 0) (reg 1) (or (reg 2) (reg 3)))))))

(defun post-or-get-parameter (name)
  (or (hunchentoot:post-parameter name) (hunchentoot:get-parameter name)))

(hunchentoot:define-easy-handler (view-root :uri "/") (offset limit sort)
				 (with-error-page
                                   (if (and sort (member sort '("new" "hot") :test #'string=))
                                       (set-user-pref :default-sort sort))
				   (let* ((offset (and offset (parse-integer offset)))
                                          (limit (and limit (parse-integer limit)))
					  (posts (get-posts-index :offset offset :limit (or limit (user-pref :items-per-page)) :sort (user-pref :default-sort))))
				     (view-items-index posts :section :frontpage :title "Frontpage posts" :hide-title t :with-offset (or offset 0)
                                                       :extra-html (lambda (out-stream)
                                                                     (page-toolbar-to-html out-stream
                                                                                           :title "Frontpage posts"
                                                                                           :new-post t)
                                                                     (sublevel-nav-to-html out-stream
                                                                                           '(("new" "New") ("hot" "Hot"))
                                                                                           (user-pref :default-sort)
                                                                                           :param-name "sort"
                                                                                           :extra-class "sort"))))))

(hunchentoot:define-easy-handler (view-index :uri "/index") (view before after offset limit sort)
                                 (with-error-page
                                   (if (and sort (member sort '("new" "hot") :test #'string=))
                                       (set-user-pref :default-sort sort))
                                   (let* ((offset (and offset (parse-integer offset)))
                                          (limit (and limit (parse-integer limit)))
                                          (posts (get-posts-index :view view :before before :after after :offset offset :limit (or limit (user-pref :items-per-page)) :sort (user-pref :default-sort)))
                                          (section (cond ((string= view "frontpage") :frontpage)
                                                         ((string= view "featured") :featured)
                                                         ((string= view "meta") :meta)
                                                         ((string= view "alignment-forum") :alignment-forum)
                                                         (t :all)))
                                          (page-title (format nil "~@(~A posts~)" section)))
                                     (view-items-index posts :section section :title page-title :with-offset (or offset 0)
                                                       :extra-html (lambda (out-stream)
                                                                     (page-toolbar-to-html out-stream
                                                                                           :title page-title
                                                                                           :new-post (if (eq section :meta) "meta" t))
                                                                     (if (string= view "new")
                                                                         (sublevel-nav-to-html out-stream
                                                                                               '(("new" "New") ("hot" "Hot"))
                                                                                               (user-pref :default-sort)
                                                                                               :param-name "sort"
                                                                                               :extra-class "sort")))))))

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

(hunchentoot:define-easy-handler (view-post-lw2-slug-link :uri (lambda (r) (match-lw2-slug-link (hunchentoot:request-uri r)))) ()
                                 (with-error-page
                                   (let ((location (convert-lw2-slug-link (hunchentoot:request-uri*))))
                                     (setf (hunchentoot:return-code*) 303
                                           (hunchentoot:header-out "Location") location))))

(hunchentoot:define-easy-handler (view-post-lw2-sequence-link :uri (lambda (r) (match-lw2-sequence-link (hunchentoot:request-uri r)))) ()
                                 (with-error-page
                                   (let ((location (convert-lw2-sequence-link (hunchentoot:request-uri*))))
                                     (setf (hunchentoot:return-code*) 303
                                           (hunchentoot:header-out "Location") location))))

(hunchentoot:define-easy-handler (view-feed :uri "/feed") ()
                                 (with-error-page
                                   (setf (hunchentoot:return-code*) 301
                                         (hunchentoot:header-out "Location") "/?format=rss")))

(hunchentoot:define-easy-handler (view-post-lw2-link :uri (lambda (r) (declare (ignore r)) (match-lw2-link (hunchentoot:request-uri*)))) ((csrf-token :request-type :post) (text :request-type :post) (parent-comment-id :request-type :post) (edit-comment-id :request-type :post) need-auth chrono)
                                 (with-error-page
                                   (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                                     (cond
                                       (text
                                         (check-csrf-token csrf-token)
                                         (let ((post-id (match-lw2-link (hunchentoot:request-uri*)))) 
                                           (assert (and lw2-auth-token (not (string= text ""))))
                                           (let* ((comment-data
                                                    (remove-if #'null
                                                               `(("body" . ,(postprocess-markdown text))
                                                                 ,(if (not edit-comment-id) `(:post-id . ,post-id))
                                                                 ,(if parent-comment-id `(:parent-comment-id . ,parent-comment-id)))))
                                                  (new-comment-id
                                                    (if edit-comment-id
                                                        (prog1 edit-comment-id
                                                          (do-lw2-comment-edit lw2-auth-token edit-comment-id comment-data))
                                                        (do-lw2-comment lw2-auth-token comment-data))))
                                             (cache-put "comment-markdown-source" new-comment-id text)
                                             (ignore-errors (get-post-comments post-id :force-revalidate t))
                                             (setf (hunchentoot:return-code*) 303
                                                   (hunchentoot:header-out "Location") (generate-post-link (match-lw2-link (hunchentoot:request-uri*)) new-comment-id)))))
                                       (t
                                        (multiple-value-bind (post-id comment-id) (match-lw2-link (hunchentoot:request-uri*))
                                          (labels ((output-comments (out-stream comments target)
                                                     (format out-stream "<div id=\"comments\">")
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
                                                           (if chrono
                                                               (comment-chrono-to-html out-stream comments)
                                                               (comment-tree-to-html out-stream (make-comment-parent-hash comments)))))
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
                                                       (comments (get-post-comments post-id))
                                                       (target-comment (find comment-id comments :key (lambda (c) (cdr (assoc :--id c))) :test #'string=))
                                                       (display-name (get-username (cdr (assoc :user-id target-comment)))))
                                                  (emit-page (out-stream :title (format nil "~A comments on ~A" display-name title) :content-class "post-page individual-thread-page")
                                                             (format out-stream "<h1>~A comments on <a href=\"~A\">~A</a></h1>"
                                                                     (encode-entities display-name)
                                                                     (generate-post-link post-id)
                                                                     (clean-text-to-html title))
                                                             (output-comments out-stream comments target-comment)
                                                             (when lw2-auth-token
                                                               (force-output out-stream)
                                                               (output-comments-votes out-stream))))
                                                (emit-page (out-stream :title title :content-class "post-page")
                                                           (cond
                                                             (condition
                                                               (error-to-html out-stream condition))
                                                             (t
                                                              (post-body-to-html out-stream post)))
                                                           (when (and lw2-auth-token (equal (logged-in-userid) (cdr (assoc :user-id post))))
                                                             (format out-stream "<div class=\"post-controls\"><a class=\"edit-post-link button\" href=\"/edit-post?post-id=~A\" accesskey=\"e\" title=\"Edit post [e]\">Edit post</a></div>"
                                                                     (cdr (assoc :--id post))))
                                                           (force-output out-stream)
                                                           (handler-case
                                                             (let ((comments (get-post-comments post-id)))
                                                               (output-comments out-stream comments nil))
                                                             (serious-condition (c) (error-to-html out-stream c)))
                                                           (when lw2-auth-token
                                                             (force-output out-stream)
                                                             (output-post-vote out-stream)
                                                             (output-comments-votes out-stream))))))))))))

(defparameter *edit-post-template* (compile-template* "edit-post.html"))

(hunchentoot:define-easy-handler (view-edit-post :uri "/edit-post") ((csrf-token :request-type :post) (text :request-type :post) title url section post-id link-post)
                                 (with-error-page
                                   (cond
                                     (text
                                       (check-csrf-token csrf-token)
                                       (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token"))
                                             (url (if (string= url "") nil url)))
                                         (assert (and lw2-auth-token (not (string= text ""))))
                                         (let* ((post-data `(("body" . ,(postprocess-markdown text)) ("title" . ,title) ("url" . ,(if link-post url))
                                                                              ("meta" . ,(string= section "meta")) ("draft" . ,(string= section "drafts"))))
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
                                             (setf (hunchentoot:return-code*) 303
                                                   (hunchentoot:header-out "Location") (if (cdr (assoc "draft" post-data :test #'equal))
                                                                                           (concatenate 'string (generate-post-link new-post-data) "?need-auth=y")
                                                                                           (generate-post-link new-post-data)))))))
                                     (t
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
                                                                     :post-id post-id
                                                                     :section-list (loop for (name desc) in '(("all" "All") ("meta" "Meta") ("drafts" "Drafts"))
                                                                                         collect (alist :name name :desc desc :selected (string= name section)))
                                                                     :markdown-source (or (and post-id (cache-get "post-markdown-source" post-id)) (cdr (assoc :html-body post-body)) ""))))))))

(hunchentoot:define-easy-handler (view-karma-vote :uri "/karma-vote") ((csrf-token :request-type :post) (target :request-type :post) (target-type :request-type :post) (vote-type :request-type :post))
				 (check-csrf-token csrf-token)
				 (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token")))
				   (multiple-value-bind (points vote-type) (do-lw2-vote lw2-auth-token target target-type vote-type)
				     (json:encode-json-to-string (list (pretty-number points "point") vote-type)))))

(hunchentoot:define-easy-handler (view-check-notifications :uri "/check-notifications") ()
                                 (with-error-page
                                   (if *current-auth-token*
                                       (let ((notifications-status (check-notifications (logged-in-userid) *current-auth-token*)))
                                         (json:encode-json-to-string notifications-status)))))

(hunchentoot:define-easy-handler (view-recent-comments :uri "/recentcomments") (offset limit)
				 (with-error-page
				   (let* ((offset (and offset (parse-integer offset)))
                                          (limit (and limit (parse-integer limit)))
					  (recent-comments (if (or offset limit (/= (user-pref :items-per-page) 20))
							     (lw2-graphql-query (lw2-query-string :comment :list
                                                                                                  (remove nil (alist :view "postCommentsNew" :limit (or limit (user-pref :items-per-page)) :offset offset)
                                                                                                          :key #'cdr)
                                                                                                  *comments-index-fields*))
							     (get-recent-comments))))
                                     (view-items-index recent-comments :title "Recent comments" :with-offset (or offset 0) :with-next t))))

(defmacro define-regex-handler (name (regex &rest vars) additional-vars &body body)
  (alexandria:with-gensyms (fn result-vector)
    `(let ((,fn (lambda (r) (ppcre:scan-to-strings ,regex (hunchentoot:request-uri r)))))
       (hunchentoot:define-easy-handler (,name :uri ,fn) ,additional-vars
	 (let ((,result-vector (nth-value 1 (funcall ,fn hunchentoot:*request*))))
	   (declare (type vector ,result-vector)) 
	   (symbol-macrolet
	     ,(loop for v in vars as x from 0 collecting `(,v (if (> (length ,result-vector) ,x) (aref ,result-vector ,x)))) 
	     ,@body))))))

(define-regex-handler view-user ("^/users/(.*?)(?:$|\\?)|^/user" user-slug) (id offset show sort)
                      (with-error-page
                        (let* ((show-text show)
                               (show (alexandria:switch (show-text :test #'string=)
                                                        ("posts" :posts) ("comments" :comments) ("drafts" :drafts)
                                                        ("conversations" :conversations) ("inbox" :inbox)))
                               (offset (if offset (parse-integer offset) 0))
                               (auth-token (if (eq show :inbox) (hunchentoot:cookie-in "lw2-auth-token")))
                               (user-query-terms (cond
                                                   (user-slug (alist :slug user-slug))
                                                   (id (alist :document-id id))))
                               (user-info
                                 (let ((ui (lw2-graphql-query (lw2-query-string :user :single user-query-terms `(:--id :display-name :karma ,@(if (eq show :inbox) '(:last-notifications-check))))
                                                              :auth-token auth-token)))
                                   (if (cdr (assoc :--id ui))
                                       ui
                                       (error (make-condition 'lw2-user-not-found-error)))))
                               (user-id (cdr (assoc :--id user-info)))
                               (own-user-page (logged-in-userid user-id))
                               (comments-index-fields (remove :page-url *comments-index-fields*)) ; page-url sometimes causes "Cannot read property '_id' of undefined" error
                               (display-name (if user-slug (cdr (assoc :display-name user-info)) user-id))
                               (title (format nil "~A~@['s ~A~]" display-name (if (member show '(nil :posts :comments)) show-text)))
                               (sort-type (alexandria:switch (sort :test #'string=) ("top" :score) (t :date)))
                               (comments-base-terms (ecase sort-type (:score (load-time-value (alist :view "postCommentsTop"))) (:date (load-time-value (alist :view "allRecentComments")))))
                               (items (case show
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
                                                                                           *comments-index-fields*))
                                                                       ("post"
                                                                        (lw2-query-string* :post :single (alist :document-id (cdr (assoc :document-id n)))
                                                                                           *posts-index-fields*))
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
                                            (do-user-edit (hunchentoot:cookie-in "lw2-auth-token") user-id (alist :last-notifications-check (local-time:format-timestring nil (local-time:now)
                                                                                                                                                                          :format lw2.graphql:+graphql-timestamp-format+
                                                                                                                                                                          :timezone local-time:+utc-zone+)))))
                                        (t
                                          (let ((user-posts (get-user-posts user-id :limit (+ 1 (user-pref :items-per-page) offset)))
                                                (user-comments (lw2-graphql-query (lw2-query-string :comment :list (nconc (alist :limit (+ 1 (user-pref :items-per-page) offset) :user-id user-id) comments-base-terms) 
                                                                                                        comments-index-fields))))
                                            (concatenate 'list user-posts user-comments)))))
                               (with-next (> (length items) (+ (if show 0 offset) (user-pref :items-per-page))))
                               (interleave (if (not show) (comment-post-interleave items :limit (user-pref :items-per-page) :offset (if show nil offset) :sort-by sort-type) (firstn items (user-pref :items-per-page))))) ; this destructively sorts items
                          (view-items-index interleave :with-offset offset :title title :content-class (format nil "user-page~@[ ~A-user-page~]" (if show show-text)) :current-uri (format nil "/users/~A" user-slug)
                                            :section :personal
                                            :with-offset offset :with-next with-next
                                            :need-auth (eq show :drafts) :section (if (eq show :drafts) "drafts" nil)
                                            :extra-html (lambda (out-stream)
                                                          (page-toolbar-to-html out-stream
                                                                                :title title
                                                                                :rss (not (member show '(:drafts :conversations :inbox)))
                                                                                :new-post (if (eq show :drafts) "drafts" t)
                                                                                :new-conversation (if own-user-page t user-slug)
                                                                                :logout own-user-page)
                                                          (format out-stream "<h1 class=\"page-main-heading\">~A</h1><div class=\"user-stats\">Karma: <span class=\"karma-total\">~A</span></div>"
                                                                  (encode-entities display-name)
                                                                  (if user-slug (pretty-number (or (cdr (assoc :karma user-info)) 0)) "##"))
                                                          (sublevel-nav-to-html out-stream
                                                                                `((nil "All") ("posts" "Posts") ("comments" "Comments")
                                                                                              ,@(if own-user-page
                                                                                                    '(("drafts" "Drafts") ("conversations" "Conversations") ("inbox" "Inbox"))))
                                                                                show-text)
                                                          (when (member show '(nil :posts :comments))
                                                            (sublevel-nav-to-html out-stream
                                                                                  `((nil "New") ("top" "Top"))
                                                                                  sort
                                                                                  :param-name "sort"
                                                                                  :extra-class "sort")))))))

(defparameter *conversation-template* (compile-template* "conversation.html"))

(hunchentoot:define-easy-handler (view-conversation :uri "/conversation") (id (csrf-token :request-type :post) (text :request-type :post))
                                 (with-error-page
                                   (let ((to (post-or-get-parameter "to")))
                                     (cond
                                       (text
                                         (check-csrf-token csrf-token)
                                         (let* ((subject (post-or-get-parameter "subject"))
                                                (id (or id
                                                        (let ((participant-ids (list (logged-in-userid) (cdar (lw2-graphql-query (lw2-query-string :user :single (alist :slug to) '(:--id)))))))
                                                          (do-create-conversation (hunchentoot:cookie-in "lw2-auth-token") (alist :participant-ids participant-ids :title subject))))))
                                           (do-create-message (hunchentoot:cookie-in "lw2-auth-token") id text)
                                           (setf (hunchentoot:return-code*) 303
                                                 (hunchentoot:header-out "Location") (format nil "/conversation?id=~A" id))))
                                       ((and id to) (error "This is an invalid URL."))
                                       (id
                                         (multiple-value-bind (conversation messages)
                                           (get-conversation-messages id (hunchentoot:cookie-in "lw2-auth-token"))
                                           (view-items-index (nreverse messages) :content-class "conversation-page" :need-auth t :title (encode-entities (postprocess-conversation-title (cdr (assoc :title conversation))))
                                                             :extra-html (with-output-to-string (out-stream) (render-template* *conversation-template* out-stream
                                                                                                                               :conversation conversation :csrf-token (make-csrf-token))))))
                                       (t
                                         (emit-page (out-stream :title "New conversation" :content-class "conversation-page")
                                                    (render-template* *conversation-template* out-stream
                                                                      :to to
                                                                      :csrf-token (make-csrf-token))))))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body
              (handler-case (markdown:parse (cdr (assoc :body item)))
                (serious-condition () "[Error while processing search result]")))
        item))

(hunchentoot:define-easy-handler (view-search :uri "/search") (q)
				 (with-error-page
				   (let ((*current-search-query* q)
					 (link (convert-any-link q)))
				     (declare (special *current-search-query*))
				     (if link
				       (setf (hunchentoot:return-code*) 301
					     (hunchentoot:header-out "Location") link)
				       (multiple-value-bind (posts comments) (lw2-search-query q)
                                         (view-items-index (nconc (map 'list (lambda (p) (if (cdr (assoc :comment-count p)) p (cons (cons :comment-count 0) p))) posts)
                                                                  (map 'list #'search-result-markdown-to-html comments))
                                                           :content-class "search-results-page" :current-uri "/search"
                                                           :title (format nil "~@[~A - ~]Search" q)))))))

(hunchentoot:define-easy-handler (view-login :uri "/login") (return cookie-check (csrf-token :request-type :post) (login-username :request-type :post) (login-password :request-type :post)
								    (signup-username :request-type :post) (signup-email :request-type :post) (signup-password :request-type :post) (signup-password2 :request-type :post))
				 (with-error-page
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
						     (with-outputs (out-stream) "<div class=\"login-tip\"><span>Tip:</span> You can log in with the same username and password that you use on LessWrong. Creating an account here also creates one on LessWrong.</div></div>")))))
				     (cond
				       ((not (or cookie-check (hunchentoot:cookie-in "session-token")))
					(hunchentoot:set-cookie "session-token" :max-age (- (expt 2 31) 1) :secure *secure-cookies* :value (base64:usb8-array-to-base64-string (ironclad:make-random-salt)))
					(setf (hunchentoot:return-code*) 303
					      (hunchentoot:header-out "Location") (format nil "/login?~@[return=~A&~]cookie-check=y" (if return (url-rewrite:url-encode return))))) 
				       (cookie-check
					 (if (hunchentoot:cookie-in "session-token")
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))))
					   (emit-page (out-stream :title "Log in" :current-uri "/login")
						      (format out-stream "<h1>Enable cookies</h1><p>Please enable cookies in your browser and <a href=\"/login~@[?return=~A~]\">try again</a>.</p>" (if return (url-rewrite:url-encode return)))))) 
				       (login-username
					 (check-csrf-token csrf-token)
					 (cond
					   ((or (string= login-username "") (string= login-password "")) (emit-login-page :error-message "Please enter a username and password")) 
					   (t (multiple-value-bind (user-id auth-token error-message expires) (do-login "username" login-username login-password)
						(cond
						  (auth-token
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :secure *secure-cookies* :max-age (and expires (+ (- expires (get-unix-time)) (* 24 60 60))))
                                                    (if expires (hunchentoot:set-cookie "lw2-status" :value (json:encode-json-to-string (alist :expires expires)) :secure *secure-cookies* :max-age (- (expt 2 31) 1)))
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token login-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[^/]" return)) return "/")))
						  (t
						    (emit-login-page :error-message error-message))))))) 
				       (signup-username
					 (check-csrf-token csrf-token)
					 (cond
					   ((not (every (lambda (x) (not (string= x ""))) (list signup-username signup-email signup-password signup-password2)))
					    (emit-login-page :error-message "Please fill in all fields"))
					   ((not (string= signup-password signup-password2))
					    (emit-login-page :error-message "Passwords do not match"))
					   (t (multiple-value-bind (user-id auth-token error-message expires) (do-lw2-create-user signup-username signup-email signup-password)
						(cond
						  (error-message (emit-login-page :error-message error-message))
                                                  (t
                                                    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :secure *secure-cookies* :max-age (+ (- expires (get-unix-time)) (* 24 60 60)))
                                                    (hunchentoot:set-cookie "lw2-status" :value (json:encode-json-to-string (alist :expires expires)) :secure *secure-cookies* :max-age (- (expt 2 31) 1))
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token signup-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[~/]" return)) return "/"))))))))
				       (t
					 (emit-login-page)))))) 

(hunchentoot:define-easy-handler (view-logout :uri "/logout") ((logout :request-type :post))
                                 (with-error-page
                                   (check-csrf-token logout)
                                   (hunchentoot:set-cookie "lw2-auth-token" :value "" :secure *secure-cookies* :max-age 0)
                                   (setf (hunchentoot:return-code*) 303
                                         (hunchentoot:header-out "Location") "/")))

(defparameter *reset-password-template* (compile-template* "reset-password.html"))

(hunchentoot:define-easy-handler (view-reset-password :uri "/reset-password") ((csrf-token :request-type :post) (email :request-type :post) (reset-link :request-type :post) (password :request-type :post) (password2 :request-type :post))
                                 (labels ((emit-rpw-page (&key message message-type step)
                                            (with-error-page
                                              (let ((csrf-token (make-csrf-token)))
                                                (emit-page (out-stream :title "Reset password" :content-class "reset-password" :robots "noindex, nofollow")
                                                           (render-template* *reset-password-template* out-stream
                                                                             :csrf-token csrf-token
                                                                             :reset-link reset-link
                                                                             :message message
                                                                             :message-type message-type
                                                                             :step step))))))
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

(hunchentoot:define-easy-handler (view-archive :uri (lambda (r) (ppcre:scan "^/archive([/?]|$)" (hunchentoot:request-uri r)))) (offset)
				 (with-error-page
				   (destructuring-bind (year month day) (map 'list (lambda (x) (if x (parse-integer x)))
									     (nth-value 1 (ppcre:scan-to-strings "^/archive(?:/(\\d{4})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))(?:/(\\d{1,2})|/?(?:$|\\?.*$))"
														 (hunchentoot:request-uri*)))) 
				     (local-time:with-decoded-timestamp (:day current-day :month current-month :year current-year) (local-time:now)
		                       (local-time:with-decoded-timestamp (:day earliest-day :month earliest-month :year earliest-year) *earliest-post*
                                         (labels ((url-elements (&rest url-elements)
                                                    (declare (dynamic-extent url-elements))
                                                    (format nil "/~{~A~^/~}" url-elements)))
                                            (let* ((offset (if offset (parse-integer offset) 0))
                                                   (posts (lw2-graphql-query (lw2-query-string :post :list
                                                                                               (alist :view (if day "new" "top") :limit 51 :offset offset
                                                                                                      :after (if (and year (not day)) (format nil "~A-~A-~A" (or year earliest-year) (or month 1) (or day 1)))
                                                                                                      :before (if year (format nil "~A-~A-~A" (or year current-year) (or month 12)
                                                                                                                               (or day (local-time:days-in-month (or month 12) (or year current-year))))))
                                                                                               *posts-index-fields*))))
                                              (emit-page (out-stream :title "Archive" :current-uri "/archive" :content-class "archive-page"
                                                                     :items-per-page 50 :with-offset offset :with-next (> (length posts) 50) :top-nav top-nav)
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
                                                (format out-stream "</div>")
                                                (funcall top-nav)
                                                (write-index-items-to-html out-stream (firstn posts 50) :empty-message "No posts for the selected period.")))))))))

(hunchentoot:define-easy-handler (view-about :uri "/about") ()
				 (with-error-page
				   (emit-page (out-stream :title "About" :current-uri "/about" :content-class "about-page")
					      (alexandria:with-input-from-file (in-stream "www/about.html" :element-type '(unsigned-byte 8))
									       (alexandria:copy-stream in-stream out-stream))))) 

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
                                                                                      '(("/theme_tweaker.css" "text/css")
                                                                                        ("/script.js" "text/javascript")
                                                                                        ("/favicon.ico" "image/x-icon")
                                                                                        ("/fa-regular-400.ttf" "font/ttf")
                                                                                        ("/fa-solid-900.ttf" "font/ttf"))
                                                                                      collect (defres uri content-type))))
                                                                   (when file
                                                                     (when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
                                                                       (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
                                                                     (hunchentoot:handle-static-file file content-type)
                                                                     t))))
                                 nil)
