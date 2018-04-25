(uiop:define-package #:lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:djula #:lw2-viewer.config #:lw2.utils #:lw2.lmdb #:lw2.backend #:lw2.links #:lw2.clean-html #:lw2.login))

(in-package #:lw2-viewer) 

(add-template-directory (asdf:system-relative-pathname "lw2-viewer" "templates/"))

(defvar *current-auth-token*) 

(defvar *memory-intensive-mutex* (sb-thread:make-mutex :name "memory-intensive-mutex")) 

(defun logged-in-userid (&optional is-userid)
  (let ((current-userid (and *current-auth-token* (cache-get "auth-token-to-userid" *current-auth-token*))))
    (if is-userid
      (string= current-userid is-userid)
      current-userid))) 
(defun logged-in-username ()
  (and *current-auth-token* (cache-get "auth-token-to-username" *current-auth-token*))) 

(defun pretty-time (timestring &key format)
  (let ((time (local-time:parse-timestring timestring)))
  (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	  (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-number (number &optional object)
  (let ((str (coerce (format nil "~A~@[<span> ~A~P</span>~]" number object number) '(vector character))))
    (if (eq (aref str 0) #\-)
      (setf (aref str 0) #\MINUS_SIGN))
    str))

(defun encode-entities (text)
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (plump:encode-entities text)))

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
                                  (list (gensym) bind (or type t) (or key (intern (string bind) '#:keyword)))))))
      (macrolet ((inner-loop (&body body)
                   `(loop for (gensym bind type key) in inner-bindings collect
                          (progn gensym bind type key ,@body))))
        `(let ,(inner-loop `(,gensym (assoc ,key ,alist)))
           (declare ,@(inner-loop `(type (or null (cons symbol ,type)) ,gensym)))
           (symbol-macrolet ,(inner-loop `(,bind (cdr ,gensym)))
             ,@body))))))

(defun post-headline-to-html (out-stream post &key need-auth)
  (alist-bind ((title string)
               (user-id string)
               (url (or null string))
               (posted-at string)
               (base-score fixnum)
               (comment-count (or null fixnum))
               (page-url (or null string)))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<h1 class=\"listing~:[~; link-post-listing~]\">~@[<a href=\"~A\">&#xf0c1;</a>~]<a href=\"~A\">~A</a></h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\"><span class=\"karma-value\">~A</span></div><a class=\"comment-count\" href=\"~A#comments\">~A comment~:P</a>~@[<a class=\"lw2-link\" href=\"~A\">LW link</a>~]~A</div>"
              url
              (if url (encode-entities (string-trim " " url)))
              (generate-post-auth-link post nil nil need-auth)
              (encode-entities (clean-text title))
              (encode-entities (get-user-slug user-id))
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              (pretty-number base-score "point")
              (generate-post-link post)
              (or comment-count 0)
              (clean-lw-link page-url)
              (if url (format nil "<div class=\"link-post-domain\">(~A)</div>" (encode-entities (puri:uri-host (puri:parse-uri (string-trim " " url))))) "")))))

(defun post-body-to-html (out-stream post)
  (alist-bind ((post-id string :--id)
               (title string)
               (user-id string)
               (url (or null string))
               (posted-at string)
               (base-score fixnum)
               (comment-count (or null fixnum))
               (page-url (or null string))
               (html-body string))
    post
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<div class=\"post~:[~; link-post~]\"><h1>~A</h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\" data-post-id=\"~A\"><span class=\"karma-value\">~A</span></div><a class=\"comment-count\" href=\"#comments\">~A comment~:P</a>~@[<a class=\"lw2-link\" href=\"~A\">LW link</a>~]<a href=\"#bottom-bar\"></a></div><div class=\"post-body\">~A</div></div>"
              url
              (encode-entities (clean-text title))
              (encode-entities (get-user-slug user-id))
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              post-id
              (pretty-number base-score "point")
              (or comment-count 0)
              (clean-lw-link page-url)
              (format nil "~A~A"
                      (if url (format nil "<p><a href=\"~A\" class=\"link-post-link\">Link post</a></p>" (encode-entities (string-trim " " url))) "")
                      (clean-html (or html-body "") :with-toc t :post-id post-id))))))

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
               (child-count fixnum)
               (children list)
               (html-body string))
    comment
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      (format out-stream "<div class=\"comment~{ ~A~}\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <a class=\"date\" href=\"~A\" data-js-date=\"~A\">~A</a><div class=\"karma\"><span class=\"karma-value\">~A</span></div>~@[<a class=\"lw2-link\" href=\"~A\">LW link</a>~]~A</div><div class=\"comment-body\"~@[ data-markdown-source=\"~A\"~]>~A</div></div>"
              (let ((l nil))
                (if (and (logged-in-userid user-id) (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000))) (push "just-posted-comment" l))
                (if highlight-new (push "comment-item-highlight" l))
                l)
              (encode-entities (get-user-slug user-id))
              (encode-entities (get-username user-id))
              (generate-post-link post-id comment-id)
              js-time
              pretty-time
              (pretty-number base-score "point")
              (clean-lw-link page-url)
              (if with-post-title
                  (format nil "<div class=\"comment-post-title\">~1{in reply to: <a href=\"/users/~A\">~A</a>’s <a href=\"~A\">comment</a> ~}on: <a href=\"~A\">~A</a></div>"
                          (alexandria:if-let (parent-comment parent-comment)
                                             (list (encode-entities (get-user-slug (cdr (assoc :user-id parent-comment))))
                                                   (encode-entities (get-username (cdr (assoc :user-id parent-comment))))
                                                   (generate-post-link (cdr (assoc :post-id parent-comment)) (cdr (assoc :comment-id parent-comment)))))
                          (generate-post-link post-id)
                          (encode-entities (clean-text (get-post-title post-id))))
                  (format nil "~@[<a class=\"comment-parent-link\" href=\"#comment-~A\">Parent</a>~]~@[<div class=\"comment-child-links\">Replies: ~:{<a href=\"#comment-~A\">&gt;~A</a>~}</div>~]~:[~;<div class=\"comment-minimize-button\" data-child-count=\"~A\"></div>~]"
                          parent-comment-id
                          (map 'list (lambda (c) (list (cdr (assoc :comment-id c)) (get-username (cdr (assoc :user-id c))))) children)
                          (not parent-comment-id)
                          child-count))
              (if (logged-in-userid user-id)
                  (encode-entities
                    (or (cache-get "comment-markdown-source" comment-id)
                        html-body)))
              (clean-html html-body)))))

(defun conversation-message-to-html (out-stream message)
  (alist-bind ((user-id string)
               (created-at string)
               (highlight-new boolean)
               (conversation list)
               (content cons))
    message
    (multiple-value-bind (pretty-time js-time) (pretty-time created-at)
      (format out-stream "<div class=\"comment private-message~A\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <span class=\"date\" data-js-date=\"~A\">~A</span><div class=\"comment-post-title\">Private message in: <a href=\"/conversation?id=~A\">~A</a></div></div><div class=\"comment-body\">~A</div></div>"
              (if highlight-new " comment-item-highlight" "")
              (encode-entities (get-user-slug user-id))
              (encode-entities (get-username user-id))
              js-time
              pretty-time
              (encode-entities (cdr (assoc :--id conversation)))
              (encode-entities (cdr (assoc :title conversation)))
              (format nil "~{<p>~A</p>~}" (loop for block in (cdr (assoc :blocks content)) collect (encode-entities (cdr (assoc :text block)))))))))

(defun conversation-index-to-html (out-stream conversation)
  (alist-bind ((conversation-id string :--id)
               (title string)
               (created-at (or null string))
               (participants list)
               (messages-total fixnum))
    conversation
    (multiple-value-bind (pretty-time js-time) (if created-at (pretty-time created-at) (values "[Error]" 0))
      (format out-stream "<h1 class=\"listing\"><a href=\"/conversation?id=~A\">~A</a></h1><div class=\"post-meta\"><div class=\"conversation-participants\"><ul>~:{<li><a href=\"/users/~A\">~A</a></li>~}</ul></div><div class=\"messages-count\">~A</div><div class=\"date\" data-js-date=\"~A\">~A</div></div>"
              (encode-entities conversation-id)
              (encode-entities title)
              (loop for p in participants
                    collect (list (encode-entities (cdr (assoc :slug p))) (encode-entities (cdr (assoc :display-name p)))))
              (pretty-number messages-total "message")
              js-time
              pretty-time))))

(defun error-to-html (out-stream condition)
  (format out-stream "<h1>Error</h1><p>~A</p>"
	  condition))

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

(defun comment-tree-to-html (out-stream comment-hash &optional (target nil) (level 0))
  (let ((comments (gethash target comment-hash)))
    (when comments
      (format out-stream "<ul class=\"comment-thread\">")
      (loop for c in comments do 
            (let ((c-id (cdr (assoc :--id c)))) 
              (format out-stream "<li id=\"comment-~A\" class=\"comment-item\">" c-id)
              (comment-to-html out-stream c)
              (if (and (= level 10) (gethash c-id comment-hash))
                  (format out-stream "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\" data-child-count=\"~A comment~:P\">Expand this thread</label>"
                          c-id
                          (cdr (assoc :child-count c))))
              (comment-tree-to-html out-stream comment-hash c-id (1+ level))
              (format out-stream "</li>")))
      (format out-stream "</ul>"))))

(defun comment-chrono-to-html (out-stream comments)
  (let ((comment-hash (make-comment-parent-hash comments)) 
        (comments-sorted (sort comments #'local-time:timestamp< :key (lambda (c) (local-time:parse-timestring (cdr (assoc :posted-at c)))))))
    (format nil "<ul class=\"comment-thread\">")
    (loop for c in comments-sorted do
          (let* ((c-id (cdr (assoc :--id c)))
                 (new-c (acons :children (gethash c-id comment-hash) c)))
            (format out-stream "<li id=\"comment-~A\" class=\"comment-item\">" c-id)
            (comment-to-html out-stream new-c)
            (format out-stream "</li>")))
    (format nil "</ul>")))

(defun comment-post-interleave (list &key limit offset)
  (let ((sorted (sort list #'local-time:timestamp> :key (lambda (x) (local-time:parse-timestring (cdr (assoc :posted-at x)))))))
    (loop for end = (if (or limit offset) (+ (or limit 0) (or offset 0)))
	  for x in sorted
	  for count from 0
	  until (and end (>= count end))
	  when (or (not offset) (>= count offset))
	  collect x)))

(defmacro with-error-html-block ((out-stream) &body body)
  `(handler-case
     (progn ,@body)
     (serious-condition (c) (error-to-html ,out-stream c))))

(defun write-index-items-to-html (out-stream items &key need-auth (empty-message "No entries."))
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
             (conversation-message-to-html out-stream x)
             (format out-stream "</li></ul>"))
            ((string= (cdr (assoc :----typename x)) "Conversation")
             (conversation-index-to-html out-stream x))
            ((assoc :comment-count x)
             (post-headline-to-html out-stream x :need-auth need-auth))
            (t
             (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item\" id=\"comment-~A\">" (cdr (assoc :--id x)))
             (comment-to-html out-stream x :with-post-title t)
             (format out-stream "</li></ul>")))))
      (format out-stream "<div class=\"listing-message\">~A</div>" empty-message)))

(defun write-index-items-to-rss (out-stream items &key title need-auth)
  (let ((full-title (format nil "~@[~A - ~]LessWrong 2 viewer" title)))
    (with-recursive-lock (*memory-intensive-mutex*)
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
                         :body (clean-html (cdr (assoc :html-body item)))))))))))

(defun check-notifications (user-id auth-token)
  (handler-case
    (multiple-value-bind (notifications user-info)
      (lw2-graphql-query-multi (list
                                 (graphql-query-string* "NotificationsList" (alist :terms (nconc (alist :user-id user-id :limit 1) *notifications-base-terms*))
                                                        '(:created-at))
                                 (graphql-query-string* "UsersSingle" (alist :document-id user-id) '(:last-notifications-check)))
			     :auth-token auth-token)
      (when (and notifications user-info)
	(local-time:timestamp> (local-time:parse-timestring (cdr (assoc :created-at (first notifications)))) (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info))))))
    (t () nil)))

(defparameter *fonts-stylesheet-uri* "//fonts.greaterwrong.com/?fonts=Charter,Concourse,a_Avante,Whitney,SourceSansPro,Raleway,ProximaNova,AnonymousPro,InputSans,GaramondPremierPro,ProximaNova,BitmapFonts")
(defparameter *fonts-stylesheet-uri* "//fonts.greaterwrong.com/?fonts=*")

(defparameter *html-head*
  (format nil
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"~A\">"
          *fonts-stylesheet-uri*))

(defun generate-versioned-link (file)
  (format nil "~A?v=~A" file (sb-posix:stat-mtime (sb-posix:stat (format nil "www~A" file))))) 

(defun search-bar-to-html ()
  (declare (special *current-search-query*))
  (let ((query (and (boundp '*current-search-query*) (hunchentoot:escape-for-html *current-search-query*))))
    (format nil "<form action=\"/search\" class=\"nav-inner\"><input name=\"q\" type=\"search\" ~@[value=\"~A\"~] autocomplete=\"off\" accesskey=\"s\" title=\"Search [s]&#10;Tip: Paste a LessWrong URL here to jump to that page.\"><button>Search</button></form>" query)))  

(defun inbox-to-html (user-slug new-messages)
  (let* ((target-uri (format nil "/users/~A?show=inbox" user-slug))
         (as-link (string= (hunchentoot:request-uri*) target-uri)))
    (multiple-value-bind (nm-class nm-text)
      (if new-messages (values "new-messages" "New messages") (values "no-messages" "Inbox"))
      (format nil "<~:[a href=\"~A\"~;span~*~] id=\"inbox-indicator\" class=\"~A\" accesskey=\"o\" title=\"~A~:[ [o]~;~]\">~A</a>"
              as-link target-uri nm-class nm-text as-link nm-text))))

(defparameter *primary-nav* '(("home" "/" "Home" :description "Latest frontpage posts" :accesskey "h")
			      ("featured" "/index?view=featured" "Featured" :description "Latest featured posts" :accesskey "f")
			      ("all" "/index?view=new&all=t" "All" :description "Latest frontpage posts and userpage posts" :accesskey "a") 
			      ("meta" "/index?view=meta&all=t" "Meta" :description "Latest meta posts" :accesskey "m")
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
         (user-id (logged-in-userid))
         (*secondary-nav* `(("archive" "/archive" "Archive" :accesskey "r")
                            ("about" "/about" "About" :accesskey "t")
                            ("search" "/search" "Search" :html ,#'search-bar-to-html)
                            ,(if username
                                 (let ((user-slug (encode-entities (get-user-slug (logged-in-userid)))))
                                   `("login" ,(format nil "/users/~A" user-slug) ,(plump:encode-entities username) :description "User page" :accesskey "u"
                                     :trailing-html ,(inbox-to-html user-slug (check-notifications user-id (hunchentoot:cookie-in "lw2-auth-token")))))
                                 `("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In" :accesskey "u")))))
    (nav-bar-to-html current-uri)))

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
  (generate-versioned-link (let ((ua (hunchentoot:header-in* :user-agent)))
                             (cond ((search "Windows" ua) "/style.windows.css")
                                   ((or (search "Linux" ua) (search "CrOS" ua) (search "Android" ua)) "/style.linux.css")
                                   (t "/style.mac.css")))))

(defun begin-html (out-stream &key title description current-uri content-class robots)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
	 (csrf-token (and session-token (make-csrf-token session-token)))) 
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head><title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~A<link rel=\"stylesheet\" href=\"~A\"><link rel=\"stylesheet\" href=\"~A\"><style id='width-adjust'></style><link rel=\"shortcut icon\" href=\"~A\"><script src=\"~A\" async></script><script src=\"~A\" async></script><script>~A</script>~@[<script>var csrfToken=\"~A\"</script>~]~@[<meta name=\"robots\" content=\"~A\">~]</head><body><div id=\"content\"~@[ class=\"~A\"~]>~A"
	    title description
	    *html-head*
	    (generate-css-link)
            (generate-versioned-link "/theme_tweaker.css")
	    (generate-versioned-link "/favicon.ico") (generate-versioned-link "/script.js") (generate-versioned-link "/guiedit.js")
	    (load-time-value (with-open-file (s "www/head.js") (uiop:slurp-stream-string s)) t)
	    csrf-token
            robots
	    content-class
            (user-nav-bar (or current-uri (replace-query-params (hunchentoot:request-uri*) "offset" nil)))))
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
                                  (setf out (remove-if (lambda (x) (equal (car x) param)) old-params)))
                           finally (return out))))
    (if new-params 
      (setf (quri:uri-query-params quri) new-params)
      (setf (quri:uri-query quri) nil))
    (quri:render-uri quri)))

(defun end-html (out-stream &key items-per-page next prev)
  (let ((request-uri (hunchentoot:request-uri*)))
    (if items-per-page (format out-stream "<script>var itemsPerPage=~A</script>" items-per-page))
    (write-string
      (nav-bar-outer "bottom-bar" nil (nav-bar-inner
					`(,@(if (and prev (> prev 0)) `(("first" ,(replace-query-params request-uri "offset" nil) "Back to first")))
					  ,@(if prev `(("prev" ,(replace-query-params request-uri "offset" (if (= prev 0) nil prev)) "Previous" :nofollow t)))
					   ("top" "#top" "Back to top")
					   ,@(if next `(("next" ,(replace-query-params request-uri "offset" next) "Next" :nofollow t))))))
      out-stream)
    (format out-stream "</div></body></html>")))

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defun call-with-emit-page (out-stream fn &key title description current-uri content-class (return-code 200) (items-per-page 20) with-offset with-next robots)
  (declare (ignore return-code))
  (ignore-errors
    (log-conditions
      (begin-html out-stream :title title :description description :current-uri current-uri :content-class content-class :robots robots)
      (funcall fn)
      (end-html out-stream
                :items-per-page (if with-offset items-per-page)
                :next (if (and with-offset with-next) (+ with-offset items-per-page)) :prev (if (and with-offset (>= with-offset items-per-page)) (- with-offset items-per-page)))
      (force-output out-stream))))

(defmacro emit-page ((out-stream &rest args &key (return-code 200) &allow-other-keys) &body body)
  (alexandria:once-only (return-code)
    (alexandria:with-gensyms (fn)
      `(progn
         (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
               (hunchentoot:return-code*) ,return-code
               (hunchentoot:header-out :link) (format nil "~:{<~A>;rel=preload;type=~A;as=~A~@{;~A~}~:^,~}"
                                                      `((,(generate-css-link) "text/css" "style")
                                                        (,*fonts-stylesheet-uri* "text/css" "style")
                                                        ("/fa-solid-900.ttf?v=1" "font/ttf" "font" "crossorigin")
                                                        ("/fa-regular-400.ttf?v=1" "font/ttf" "font" "crossorigin")
                                                        ("//fonts.greaterwrong.com/font_files/BitmapFonts/MSSansSerif.ttf" "font/ttf" "font" "crossorigin"))))
         (let* ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8))
                (,fn (lambda () ,@body)))
           (call-with-emit-page ,out-stream ,fn ,@args))))))

(defmacro with-error-page (&body body)
  `(let ((*current-auth-token* (hunchentoot:cookie-in "lw2-auth-token")))
     (handler-case
       (log-conditions 
         (progn ,@body))
       (serious-condition (condition)
                          (emit-page (out-stream :title "Error" :return-code 500) 
                                     (error-to-html out-stream condition))))))

(defun output-form (out-stream method action id class csrf-token fields button-label &key textarea end-html)
  (format out-stream "<form method=\"~A\" action=\"~A\" id=\"~A\" class=\"~A\"><div>" method action id class)
  (loop for (id label type . params) in fields
	do (format out-stream "<div><label for=\"~A\">~A:</label>" id label)
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
	do (format out-stream "</div>"))
  (if textarea
    (destructuring-bind (ta-name ta-contents) textarea
      (format out-stream "<div class=\"textarea-container\"><textarea name=\"~A\">~A</textarea><span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span></div>" ta-name (encode-entities ta-contents))))
  (format out-stream "<div class=\"action-container\"><input type=\"hidden\" name=\"csrf-token\" value=\"~A\"><input type=\"submit\" value=\"~A\">~@[~A~]</div></div></form>"
	  csrf-token button-label end-html))

(defun view-items-index (items &key section with-offset (with-next t) title current-uri hide-title need-auth hide-rss extra-html page-toolbar-extra (content-class "index-page"))
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
                     ("rss" 
                      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
                      (let ((out-stream (hunchentoot:send-headers)))
                        (write-index-items-to-rss (make-flexi-stream out-stream :external-format :utf-8) items :title title)))
                     (t
                       (emit-page (out-stream :title (if hide-title nil title) :description "A faster way to browse LessWrong 2.0" :content-class content-class :with-offset with-offset :with-next with-next
                                              :current-uri current-uri :robots (if (and with-offset (> with-offset 0)) "noindex, nofollow"))
                                  (format out-stream "<div class=\"page-toolbar\">~@[<a class=\"new-post button\" href=\"/edit-post?section=~A\" accesskey=\"n\" title=\"Create new post [n]\">New post</a>~]~@[~A~]~{~@[<a class=\"rss\" rel=\"alternate\" type=\"application/rss+xml\" title=\"~A RSS feed\" href=\"~A\">RSS</a>~]~}</div>~@[~A~]"
                                          (if (and section (logged-in-userid)) section)
                                          page-toolbar-extra
                                          (unless (or need-auth hide-rss)
                                            (list title (replace-query-params (hunchentoot:request-uri*) "offset" nil "format" "rss")))
                                          extra-html)
                                  (write-index-items-to-html out-stream items :need-auth need-auth)))))

(defun link-if-not (stream linkp url class text)
  (declare (dynamic-extent linkp url text))
  (if (not linkp)
      (format stream "<a href=\"~A\" class=\"~A\">~A</a>" url class text)
      (format stream "<span class=\"~A\">~A</span>" class text)))

(defun postprocess-markdown (markdown)
  (ppcre:regex-replace-all (load-time-value (concatenate 'string (ppcre:regex-replace-all "\\." *site-uri* "\\.") "posts/([^/ ]{17})/([^/# ]*)(?:(#)comment-([^/ ]{17}))?"))
                           markdown
                           "https://www.lesserwrong.com/posts/\\1/\\2\\3\\4"))

(defun post-or-get-parameter (name)
  (or (hunchentoot:post-parameter name) (hunchentoot:get-parameter name)))

(hunchentoot:define-easy-handler (view-root :uri "/") (offset)
				 (with-error-page
				   (let* ((offset (and offset (parse-integer offset)))
					  (posts (if offset
						   (lw2-graphql-query (graphql-query-string "PostsList"
											    (alist :terms (alist :view "frontpage-rss" :limit 20 :offset offset))
											    *posts-index-fields*))
						   (get-posts))))
				     (view-items-index posts :section "frontpage" :title "Frontpage posts" :hide-title t :with-offset (or offset 0)))))

(hunchentoot:define-easy-handler (view-index :uri "/index") (view meta before after offset)
                                 (with-error-page
                                   (let* ((offset (and offset (parse-integer offset))) 
                                          (posts (lw2-graphql-query (graphql-query-string "PostsList" (alist :terms
                                                                                                             (remove-if (lambda (x) (null (cdr x)))
                                                                                                                        (alist :view (if (string= view "featured") "curated" (or view "new"))
                                                                                                                               :meta (not (not meta)) :before before :after after :limit 20 :offset offset)))
                                                                                          *posts-index-fields*)))
                                          (section (or (if (string= view "new") "all" view) "all")))
                                     (view-items-index posts :section (if (string/= section "featured") section) :title (format nil "~@(~A posts~)" section) :with-offset (or offset 0)))))

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

(hunchentoot:define-easy-handler (view-feed :uri "/feed") (view meta before after)
				 (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
				 (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :meta (not (not meta)) :before before :after after)))
				       (out-stream (hunchentoot:send-headers)))
				   (write-index-items-to-rss (make-flexi-stream out-stream :external-format :utf-8) posts))) 

(hunchentoot:define-easy-handler (view-post-lw2-link :uri (lambda (r) (declare (ignore r)) (match-lw2-link (hunchentoot:request-uri*)))) ((csrf-token :request-type :post) (text :request-type :post) (parent-comment-id :request-type :post) (edit-comment-id :request-type :post) need-auth chrono)
                                 (with-error-page
                                   (cond
                                     (text
                                       (check-csrf-token csrf-token)
                                       (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token"))
                                             (post-id (match-lw2-link (hunchentoot:request-uri*)))) 
                                         (assert (and lw2-auth-token (not (string= text ""))))
                                         (let* ((comment-data `(("body" . ,(postprocess-markdown text)) ,(if (not edit-comment-id) `("postId" . ,post-id)) ,(if parent-comment-id `("parentCommentId" . ,parent-comment-id)) ("content" . ("blocks" . nil)))) 
                                                (new-comment-id
                                                  (if edit-comment-id
                                                      (prog1 edit-comment-id
                                                        (do-lw2-comment-edit lw2-auth-token edit-comment-id comment-data))
                                                      (do-lw2-comment lw2-auth-token comment-data))))
                                           (cache-put "comment-markdown-source" new-comment-id text)
                                           (get-post-comments post-id :force-revalidate t)
                                           (setf (hunchentoot:return-code*) 303
                                                 (hunchentoot:header-out "Location") (generate-post-link (match-lw2-link (hunchentoot:request-uri*)) new-comment-id)))))
                                     (t 
                                      (multiple-value-bind (post-id comment-id) (match-lw2-link (hunchentoot:request-uri*))
                                        (if comment-id 
                                            (setf (hunchentoot:return-code*) 303
                                                  (hunchentoot:header-out "Location") (generate-post-link post-id comment-id))
                                            (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                                              (multiple-value-bind (post title condition)
                                                (handler-case (get-post-body post-id :auth-token (and need-auth lw2-auth-token))
                                                  (serious-condition (c) (values nil "Error" c))
                                                  (:no-error (post) (values post (cdr (assoc :title post)) nil)))
                                                (emit-page (out-stream :title title :content-class "post-page")
                                                           (cond
                                                             (condition
                                                               (error-to-html out-stream condition))
                                                             (t
                                                              (post-body-to-html out-stream post)
                                                              (if lw2-auth-token
                                                                  (format out-stream "<script>postVote=~A</script>~@[<div class=\"post-controls\"><a class=\"edit-post-link button\" href=\"/edit-post?post-id=~A\" accesskey=\"e\" title=\"Edit post [e]\">Edit post</a></div>~]"
                                                                          (json:encode-json-to-string (get-post-vote post-id lw2-auth-token))
                                                                          (if (equal (logged-in-userid) (cdr (assoc :user-id post))) (cdr (assoc :--id post)))))))
                                                           (force-output out-stream)
                                                           (handler-case
                                                             (progn
                                                               (format out-stream "<div id=\"comments\">")
                                                               (let ((comments (get-post-comments post-id)))
                                                                 (with-cache-transaction
                                                                   (if chrono
                                                                       (comment-chrono-to-html out-stream comments)
                                                                       (comment-tree-to-html out-stream (make-comment-parent-hash comments)))))
                                                               (format out-stream "</div>")
                                                               (if lw2-auth-token
                                                                   (format out-stream "<script>commentVotes=~A</script>" (json:encode-json-to-string (get-post-comments-votes post-id lw2-auth-token)))))
                                                             (serious-condition (c) (error-to-html out-stream c))))))))))))

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
                                                                              ("frontpageDate" . ,(if (string= section "frontpage") (local-time:format-timestring nil (local-time:now))))
                                                                              ("meta" . ,(string= section "meta")) ("draft" . ,(string= section "drafts")) ("content" . ("blocks" . nil))))
                                                (post-set (loop for item in post-data when (cdr item) collect item))
                                                (post-unset (loop for item in post-data when (not (cdr item)) collect (cons (car item) t))))
                                           (let* ((new-post-data
                                                    (if post-id
                                                        (do-lw2-post-edit lw2-auth-token post-id post-set post-unset)
                                                        (do-lw2-post lw2-auth-token post-set)))
                                                  (new-post-id (cdr (assoc :--id new-post-data))))
                                             (assert new-post-id)
                                             (cache-put "post-markdown-source" new-post-id text)
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
                                                                     :section-list (loop for (name desc) in '(("frontpage" "Frontpage") ("all" "All") ("meta" "Meta") ("drafts" "Drafts"))
                                                                                         collect (alist :name name :desc desc :selected (string= name section)))
                                                                     :markdown-source (or (and post-id (cache-get "post-markdown-source" post-id)) (cdr (assoc :html-body post-body)) ""))))))))

(hunchentoot:define-easy-handler (view-karma-vote :uri "/karma-vote") ((csrf-token :request-type :post) (target :request-type :post) (target-type :request-type :post) (vote-type :request-type :post))
				 (check-csrf-token csrf-token)
				 (let ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token")))
				   (multiple-value-bind (points vote-type) (do-lw2-vote lw2-auth-token target target-type vote-type)
				     (json:encode-json-to-string (list (pretty-number points "point") vote-type)))))

(hunchentoot:define-easy-handler (view-recent-comments :uri "/recentcomments") (offset)
				 (with-error-page
				   (let* ((offset (and offset (parse-integer offset))) 
					  (recent-comments (if offset
							     (lw2-graphql-query (graphql-query-string "CommentsList"
												      (alist :terms (alist :view "postCommentsNew" :limit 20 :offset offset))
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

(define-regex-handler view-user ("^/users/(.*?)(?:$|\\?)" user-slug) (offset show)
                      (with-error-page
                        (let* ((offset (if offset (parse-integer offset) 0))
                               (auth-token (if (string= show "inbox") (hunchentoot:cookie-in "lw2-auth-token")))
                               (user-info (lw2-graphql-query (graphql-query-string "UsersSingle" (alist :slug user-slug) `(:--id :display-name :karma ,@(if (string= show "inbox") '(:last-notifications-check))))
                                                             :auth-token auth-token))
                               (comments-index-fields (remove :page-url *comments-index-fields*)) ; page-url sometimes causes "Cannot read property '_id' of undefined" error
                               (title (format nil "~A~@['s ~A~]" (cdr (assoc :display-name user-info)) (if (member show '(nil "posts" "comments") :test #'equal) show)))
                               (posts-base-terms (load-time-value (alist :view "userPosts" :meta :null)))
                               (comments-base-terms (load-time-value (alist :view "allRecentComments")))
                               (items (alexandria:switch (show :test #'string=)
                                                         ("posts"
                                                          (lw2-graphql-query (graphql-query-string "PostsList"
                                                                                                   (alist :terms (nconc (alist :offset offset :limit 21 :user-id (cdr (assoc :--id user-info))) posts-base-terms))
                                                                                                   *posts-index-fields*)))
                                                         ("comments"
                                                          (lw2-graphql-query (graphql-query-string "CommentsList"
                                                                                                   (alist :terms (nconc (alist :offset offset :limit 21 :user-id (cdr (assoc :--id user-info)))
                                                                                                                        comments-base-terms))
                                                                                                   comments-index-fields)))
                                                         ("drafts"
                                                          (lw2-graphql-query (graphql-query-string "PostsList"
                                                                                                   (alist :terms (alist :view "drafts" :limit 21 :offset offset :user-id (cdr (assoc :--id user-info))))
                                                                                                   *posts-index-fields*)
                                                                             :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                                                         ("conversations"
                                                          (let ((conversations
                                                                  (lw2-graphql-query (graphql-query-string "ConversationsList"
                                                                                                           (alist :terms (alist :view "userConversations" :limit 21 :offset offset :user-id (cdr (assoc :--id user-info))))
                                                                                                           '(:--id :created-at :title (:participants :display-name :slug) :----typename))
                                                                                     :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                                                            (lw2-graphql-query-map
                                                              (lambda (c)
                                                                (graphql-query-string* "MessagesTotal" (alist :terms (alist :view "messagesConversation" :conversation-id (cdr (assoc :--id c)))) nil))
                                                              conversations
                                                              :postprocess (lambda (c result)
                                                                             (acons :messages-total result c))
                                                              :auth-token (hunchentoot:cookie-in "lw2-auth-token"))))
                                                         ("inbox"
                                                          (prog1
                                                            (let ((notifications (lw2-graphql-query (graphql-query-string "NotificationsList"
                                                                                                                          (alist :terms (nconc (alist :user-id (cdr (assoc :--id user-info)) :limit 21 :offset offset) *notifications-base-terms*))
                                                                                                                          '(:--id :document-type :document-id :link :title :message :type :viewed))
                                                                                                    :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                                                                  (last-check (ignore-errors (local-time:parse-timestring (cdr (assoc :last-notifications-check user-info))))))
                                                              (labels ((check-new (key obj)
                                                                         (if (ignore-errors (local-time:timestamp< last-check (local-time:parse-timestring (cdr (assoc key obj)))))
                                                                             (acons :highlight-new t obj)
                                                                             obj)))
                                                                (lw2-graphql-query-map
                                                                  (lambda (n)
                                                                    (alexandria:switch ((cdr (assoc :document-type n)) :test #'string=)
                                                                                       ("comment"
                                                                                        (graphql-query-string* "CommentsSingle"
                                                                                                               (alist :document-id (cdr (assoc :document-id n)))
                                                                                                               *comments-index-fields*))
                                                                                       ("post"
                                                                                        (graphql-query-string* "PostsSingle" (alist :document-id (cdr (assoc :document-id n)))
                                                                                                               *posts-index-fields*))
                                                                                       ("message"
                                                                                        (graphql-query-string* "MessagesSingle" (alist :document-id (cdr (assoc :document-id n)))
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
                                                            (do-lw2-post-query (hunchentoot:cookie-in "lw2-auth-token")
                                                                               (list (alist :query "mutation usersEdit($documentId: String, $set: UsersInput) { usersEdit(documentId: $documentId, set: $set) { _id }}"
                                                                                            :variables (alist :document-id (cdr (assoc :--id user-info))
                                                                                                              :set (alist :last-notifications-check (local-time:format-timestring nil (local-time:now))))
                                                                                            :operation-name "usersEdit")))))
                                                         (t
                                                           (let ((user-posts (lw2-graphql-query (graphql-query-string "PostsList" (alist :terms (nconc (alist :limit (+ 21 offset) :user-id (cdr (assoc :--id user-info))) posts-base-terms)) *posts-index-fields*)))
                                                                 (user-comments (lw2-graphql-query (graphql-query-string "CommentsList" (alist :terms (nconc (alist :limit (+ 21 offset) :user-id (cdr (assoc :--id user-info))) comments-base-terms)) 
                                                                                                                         comments-index-fields))))
                                                             (concatenate 'list user-posts user-comments)))))
                               (with-next (> (length items) (+ (if show 0 offset) 20)))
                               (interleave (if (not show) (comment-post-interleave items :limit 20 :offset (if show nil offset)) (firstn items 20)))) ; this destructively sorts items
                          (view-items-index interleave :with-offset offset :title title :content-class "user-page" :current-uri (format nil "/users/~A" user-slug)
                                            :with-offset offset :with-next with-next
                                            :need-auth (string= show "drafts") :section (if (string= show "drafts") "drafts" nil)
                                            :hide-rss (some (lambda (x) (string= show x)) '("drafts" "conversations" "inbox"))
                                            :page-toolbar-extra (let ((liu (logged-in-userid)))
                                                                  (if (and liu (not (string= liu (cdr (assoc :--id user-info)))))
                                                                      (format nil "<a class=\"new-private-message button\" href=\"/conversation?to=~A\">Send private message</a>"
                                                                              user-slug)))
                                            :extra-html (format nil "<h1 class=\"page-main-heading\">~A</h1><div class=\"user-stats\">Karma: <span class=\"karma-total\">~A</span></div><div class=\"sublevel-nav\">~A</div>"
                                                                (cdr (assoc :display-name user-info))
                                                                (pretty-number (or (cdr (assoc :karma user-info)) 0))
                                                                (with-output-to-string (stream)
                                                                  (loop for (l-show text) in `((nil "All") ("posts" "Posts") ("comments" "Comments")
                                                                                                           ,@(if (logged-in-userid (cdr (assoc :--id user-info)))
                                                                                                                 '(("drafts" "Drafts") ("conversations" "Conversations") ("inbox" "Inbox"))))
                                                                        do (link-if-not stream (string= show l-show) (format nil "~A~@[?show=~A~]" (hunchentoot:script-name*) l-show)
                                                                                        "sublevel-item" text))))))))

(defparameter *conversation-template* (compile-template* "conversation.html"))

(hunchentoot:define-easy-handler (view-conversation :uri "/conversation") (id (csrf-token :request-type :post) (text :request-type :post))
                                 (with-error-page
                                   (let ((to (post-or-get-parameter "to")))
                                     (cond
                                       (text
                                         (check-csrf-token csrf-token)
                                         (let* ((subject (post-or-get-parameter "subject"))
                                                (id (or id
                                                        (let ((participant-ids (list (logged-in-userid) (cdar (lw2-graphql-query (graphql-query-string "UsersSingle" (alist :slug to) '(:--id)))))))
                                                          (do-lw2-post-query* (hunchentoot:cookie-in "lw2-auth-token")
                                                                              (list (alist :query "mutation ConversationsNew($document: ConversationsInput) { ConversationsNew(document: $document) { _id }}"
                                                                                           :variables (alist :document
                                                                                                             (alist :participant-ids participant-ids
                                                                                                                    :title subject))
                                                                                           :operation-name "ConversationsNew")))))))
                                           (do-lw2-post-query (hunchentoot:cookie-in "lw2-auth-token")
                                                              (list (alist :query "mutation MessagesNew($document: MessagesInput) { MessagesNew(document: $document) { _id }}"
                                                                           :variables (alist :document
                                                                                             (alist :content
                                                                                                    (alist :blocks (loop for para in (ppcre:split "\\n+" text)
                                                                                                                         collect (alist :text para :type "unstyled"))
                                                                                                           :entity-map (make-hash-table))
                                                                                                    :conversation-id id))
                                                                           :operation-name "MessagesNew")))
                                           (setf (hunchentoot:return-code*) 303
                                                 (hunchentoot:header-out "Location") (format nil "/conversation?id=~A" id))))
                                       ((and id to) (error "This is an invalid URL."))
                                       (id
                                         (multiple-value-bind (conversation messages)
                                           (lw2-graphql-query-multi
                                             (list
                                               (graphql-query-string* "ConversationsSingle" (alist :document-id id) '(:title (:participants :display-name :slug)))
                                               (graphql-query-string* "MessagesList" (alist :terms (alist :view "messagesConversation" :conversation-id id)) *messages-index-fields*))
                                             :auth-token (hunchentoot:cookie-in "lw2-auth-token"))
                                           (view-items-index (nreverse messages) :content-class "conversation-page" :need-auth t :title (cdr (assoc :title conversation))
                                                             :extra-html (with-output-to-string (out-stream) (render-template* *conversation-template* out-stream
                                                                                                                               :conversation conversation :csrf-token (make-csrf-token))))))
                                       (t
                                         (emit-page (out-stream :title "New conversation" :content-class "conversation-page")
                                                    (render-template* *conversation-template* out-stream
                                                                      :to to
                                                                      :csrf-token (make-csrf-token))))))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body (markdown:parse (cdr (assoc :body item)))) item)) 

(hunchentoot:define-easy-handler (view-search :uri "/search") (q)
				 (with-error-page
				   (let ((*current-search-query* q)
					 (link (or (convert-lw2-link q) (convert-lw2-slug-link q) (convert-lw2-sequence-link q) (convert-lw1-link q))))
				     (declare (special *current-search-query*))
				     (if link
				       (setf (hunchentoot:return-code*) 301
					     (hunchentoot:header-out "Location") link)
				       (multiple-value-bind (posts comments) (lw2-search-query q)
                                         (emit-page (out-stream :title "Search" :current-uri "/search" :content-class "search-results-page")
                                                    (dolist (p posts) (post-headline-to-html out-stream p))
                                                    (with-outputs (out-stream) "<ul class=\"comment-thread\">")
                                                    (dolist (c comments)
                                                      (format out-stream "<li class=\"comment-item\">")
                                                      (comment-to-html out-stream (search-result-markdown-to-html c) :with-post-title t)
                                                      (format out-stream "</li>"))
                                                    (with-outputs (out-stream) "</ul>")))))))

(hunchentoot:define-easy-handler (view-login :uri "/login") (return cookie-check (csrf-token :request-type :post) (login-username :request-type :post) (login-password :request-type :post)
								    (signup-username :request-type :post) (signup-email :request-type :post) (signup-password :request-type :post) (signup-password2 :request-type :post))
				 (with-error-page
				   (labels
				     ((emit-login-page (&key error-message)
					(let ((csrf-token (make-csrf-token)))
					  (emit-page (out-stream :title "Log in" :current-uri "/login" :content-class "login-page")
						     (when error-message
						       (format out-stream "<div class=\"error-box\">~A</div>" error-message)) 
						     (with-outputs (out-stream) "<div class=\"login-container\"><div id=\"login-form-container\"><h1>Log in</h1>")
						     (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "login-form" "aligned-form" csrf-token
								  '(("login-username" "Username" "text" "username")
								    ("login-password" "Password" "password" "current-password"))
								  "Log in"
                                                                  :end-html "<a href=\"/reset-password\">Forgot password</a>")
						     (with-outputs (out-stream) "</div><div id=\"create-account-form-container\"><h1>Create account</h1>")
						     (output-form out-stream "post" (format nil "/login~@[?return=~A~]" (if return (url-rewrite:url-encode return))) "signup-form" "aligned-form" csrf-token
								  '(("signup-username" "Username" "text" "username")
								    ("signup-email" "Email" "text" "email")
								    ("signup-password" "Password" "password" "new-password")
								    ("signup-password2" "Confirm password" "password" "new-password"))
								  "Create account")
						     (with-outputs (out-stream) "</div></div>"))))) 
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
					   (t (multiple-value-bind (user-id auth-token error-message) (do-lw2-login "username" login-username login-password) 
						(cond
						  (auth-token
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :secure *secure-cookies* :max-age (- (expt 2 31) 1)) 
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
					   (t (multiple-value-bind (user-id auth-token error-message) (do-lw2-create-user signup-username signup-email signup-password)
						(cond
						  (error-message (emit-login-page :error-message error-message))
						  (t
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :secure *secure-cookies* :max-age (- (expt 2 31) 1))
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token signup-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[~/]" return)) return "/"))))))))
				       (t
					 (emit-login-page)))))) 

(defparameter *reset-password-template* (compile-template* "reset-password.html"))

(hunchentoot:define-easy-handler (view-reset-password :uri "/reset-password") ((csrf-token :request-type :post) (email :request-type :post) (reset-link :request-type :post) (password :request-type :post) (password2 :request-type :post))
                                 (labels ((emit-rpw-page (&key message message-type step)
                                            (with-error-page
                                              (let ((csrf-token (make-csrf-token)))
                                                (emit-page (out-stream :title "Reset password" :content-class "reset-password")
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
                                                   (posts (lw2-graphql-query (graphql-query-string "PostsList"
                                                                                                   (alist :terms (alist :view (if day "new" "best") :limit 51 :offset offset
                                                                                                                        :after (if (and year (not day)) (format nil "~A-~A-~A" (or year earliest-year) (or month 1) (or day 1)))
                                                                                                                        :before (if year (format nil "~A-~A-~A" (or year current-year) (or month 12)
                                                                                                                                                 (or day (local-time:days-in-month (or month 12) (or year current-year)))))))
                                                                                                   *posts-index-fields*))))
                                              (emit-page (out-stream :title "Archive" :current-uri "/archive" :content-class "archive-page" :items-per-page 50 :with-offset offset :with-next (> (length posts) 50))
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
                                                                                        ("/guiedit.js" "text/javascript")
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
