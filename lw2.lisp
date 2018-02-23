(defpackage #:lw2-viewer
  (:use #:cl #:sb-thread #:flexi-streams #:djula #:lw2-viewer.config #:lw2.lmdb #:lw2.backend #:lw2.links #:lw2.clean-html #:lw2.login))

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

(defun post-headline-to-html (post &key need-auth)
  (multiple-value-bind (pretty-time js-time) (pretty-time (cdr (assoc :posted-at post))) 
    (format nil "<h1 class=\"listing~:[~; link-post-listing~]\">~@[<a href=\"~A\">&#xf0c1;</a>~]<a href=\"~A\">~A</a></h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\"><span class=\"karma-value\">~A</span></div><a class=\"comment-count\" href=\"~A#comments\">~A comment~:P</a>~@[<a class=\"lw2-link\" href=\"~A\">LW2 link</a>~]~A</div>"
	    (cdr (assoc :url post))
	    (if (cdr (assoc :url post)) (encode-entities (string-trim " " (cdr (assoc :url post)))))
            (generate-post-auth-link post nil nil need-auth)
	    (encode-entities (clean-text (cdr (assoc :title post))))
	    (encode-entities (get-user-slug (cdr (assoc :user-id post)))) 
	    (encode-entities (get-username (cdr (assoc :user-id post))))
	    js-time
	    pretty-time
	    (pretty-number (cdr (assoc :base-score post)) "point")
	    (generate-post-link post) 
	    (or (cdr (assoc :comment-count post)) 0) 
	    (cdr (assoc :page-url post))
	    (if (cdr (assoc :url post)) (format nil "<div class=\"link-post-domain\">(~A)</div>" (encode-entities (puri:uri-host (puri:parse-uri (string-trim " " (cdr (assoc :url post))))))) "")))) 

(defun post-body-to-html (post)
  (multiple-value-bind (pretty-time js-time) (pretty-time (cdr (assoc :posted-at post))) 
    (format nil "<div class=\"post~:[~; link-post~]\"><h1>~A</h1><div class=\"post-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <div class=\"date\" data-js-date=\"~A\">~A</div><div class=\"karma\" data-post-id=\"~A\"><span class=\"karma-value\">~A</span></div><a class=\"comment-count\" href=\"#comments\">~A comment~:P</a>~@[<a class=\"lw2-link\" href=\"~A\">LW2 link</a>~]<a href=\"#bottom-bar\"></a></div><div class=\"post-body\">~A</div></div>"
	    (cdr (assoc :url post))
	    (encode-entities (clean-text (cdr (assoc :title post))))
	    (encode-entities (get-user-slug (cdr (assoc :user-id post)))) 
	    (encode-entities (get-username (cdr (assoc :user-id post))))
	    js-time
	    pretty-time
	    (cdr (assoc :--id post)) 
	    (pretty-number (cdr (assoc :base-score post)) "point")
	    (or (cdr (assoc :comment-count post)) 0) 
	    (cdr (assoc :page-url post)) 
	    (format nil "~A~A"
		    (if (cdr (assoc :url post)) (format nil "<p><a href=\"~A\" class=\"link-post-link\">Link post</a></p>" (encode-entities (string-trim " " (cdr (assoc :url post))))) "")
		    (clean-html (or (cdr (assoc :html-body post)) "") :with-toc t :post-id (cdr (assoc :--id post))))))) 

(defun comment-to-html (comment &key with-post-title)
  (multiple-value-bind (pretty-time js-time) (pretty-time (cdr (assoc :posted-at comment))) 
    (format nil "<div class=\"comment~A\"><div class=\"comment-meta\"><a class=\"author\" href=\"/users/~A\">~A</a> <a class=\"date\" href=\"~A\" data-js-date=\"~A\">~A</a><div class=\"karma\"><span class=\"karma-value\">~A</span></div>~@[<a class=\"lw2-link\" href=\"~A\">LW2 link</a>~]~A</div><div class=\"comment-body\"~@[ data-markdown-source=\"~A\"~]>~A</div></div>"
            (if (and (logged-in-userid (cdr (assoc :user-id comment))) (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000))) " just-posted-comment" "")
	    (encode-entities (get-user-slug (cdr (assoc :user-id comment)))) 
	    (encode-entities (get-username (cdr (assoc :user-id comment)))) 
	    (generate-post-link (cdr (assoc :post-id comment)) (cdr (assoc :--id comment)))
	    js-time
	    pretty-time
	    (pretty-number (cdr (assoc :base-score comment)) "point")
	    (cdr (assoc :page-url comment)) 
	    (if with-post-title
	      (format nil "<div class=\"comment-post-title\">on: <a href=\"~A\">~A</a></div>"
		      (generate-post-link (cdr (assoc :post-id comment)))
		      (encode-entities (clean-text (get-post-title (cdr (assoc :post-id comment))))))
	      (format nil "~@[<a class=\"comment-parent-link\" href=\"#comment-~A\">Parent</a>~]~@[<div class=\"comment-child-links\">Replies: ~:{<a href=\"#comment-~A\">&gt;~A</a>~}</div>~]~:[~;<div class=\"comment-minimize-button\" data-child-count=\"~A\"></div>~]"
		      (cdr (assoc :parent-comment-id comment))
		      (map 'list (lambda (c) (list (cdr (assoc :--id c)) (get-username (cdr (assoc :user-id c))))) (cdr (assoc :children comment)))
		      (not (cdr (assoc :parent-comment-id comment)))
		      (cdr (assoc :child-count comment))))
	    (if (logged-in-userid (cdr (assoc :user-id comment)))
	      (encode-entities
		(or (cache-get "comment-markdown-source" (cdr (assoc :--id comment))) 
		    (cdr (assoc :html-body comment)))))
	    (clean-html (cdr (assoc :html-body comment)))))) 

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
			   (format nil "<li id=\"comment-~A\" class=\"comment-item\">~A~A~A</li>"
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

(defun comment-chrono-to-html (comments)
  (let ((comment-hash (make-comment-parent-hash comments)) 
	(comments-sorted (sort comments #'local-time:timestamp< :key (lambda (c) (local-time:parse-timestring (cdr (assoc :posted-at c)))))))
    (format nil "<ul class=\"comment-thread\">~{~A~}</ul>"
	    (map 'list (lambda (c)
			 (let* ((c-id (cdr (assoc :--id c)))
				(new-c (acons :children (gethash c-id comment-hash) c)))
			   (format nil "<li id=\"comment-~A\" class=\"comment-item\">~A</li>"
				   c-id
				   (comment-to-html new-c))))
		 comments-sorted))))

(defun comment-post-interleave (list &key limit offset)
  (let ((sorted (sort list #'local-time:timestamp> :key (lambda (x) (local-time:parse-timestring (cdr (assoc :posted-at x)))))))
    (loop for end = (if (or limit offset) (+ (or limit 0) (or offset 0)))
	  for x in sorted
	  for count from 0
	  until (and end (>= count end))
	  when (or (not offset) (>= count offset))
	  collect x)))

(defun write-index-items-to-html (out-stream items &key need-auth)
  (dolist (x items)
    (if (assoc :comment-count x)
        (write-string (post-headline-to-html x :need-auth need-auth) out-stream)
        (format out-stream "<ul class=\"comment-thread\"><li class=\"comment-item\" id=\"comment-~A\">~A</li></ul>"
                (cdr (assoc :--id x)) (comment-to-html x :with-post-title t)))))

(defun write-index-items-to-rss (out-stream items &key title need-auth)
  (let ((full-title (format nil "~@[~A - ~]LessWrong 2 viewer" title)))
    (with-recursive-lock (*memory-intensive-mutex*)
      (xml-emitter:with-rss2 (out-stream :encoding "UTF-8")
        (xml-emitter:rss-channel-header full-title *site-uri* :description full-title)
        (labels ((emit-item (item &key title link (author (get-username (cdr (assoc :user-id item)))) (date (pretty-time (cdr (assoc :posted-at item)) :format local-time:+rfc-1123-format+)) body)
                   (xml-emitter:rss-item
                     title
                     :link link
                     :author author
                     :pubDate date
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

(defparameter *html-head*
"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link rel=\"stylesheet\" href=\"//fonts.greaterwrong.com/?fonts=Charter,Concourse,a_Avante,Whitney,SourceSansPro,Raleway,ProximaNova,AnonymousPro,InputSans,BitmapFonts\">")

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
				("about" "/about" "About")
				("search" "/search" "Search" :html ,#'search-bar-to-html)
				("login" "/login" "Log In"))) 
(defun nav-bar-inner (items &optional current-uri)
  (let ((active-bar nil)) 
    (values
      (format nil "~{~A~}"
	      (maplist (lambda (items)
			 (let ((item (first items))) 
			   (destructuring-bind (id uri name &key description html accesskey nofollow) item
			     (if (string= uri current-uri)
			       (progn (setf active-bar t)
				      (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-current\" ~@[title=\"~A\"~]>~:[<span class=\"nav-inner\">~A</span>~;~:*~A~]</span>"
					      id description (and html (funcall html)) name)) 
			       (format nil "<span id=\"nav-item-~A\" class=\"nav-item nav-inactive~:[~; nav-item-last-before-current~]\" ~@[title=\"~A\"~]>~:[<a href=\"~A\" class=\"nav-inner\"~@[ accesskey=\"~A\"~]~:[~; rel=\"nofollow\"~]>~A</a>~;~:*~A~]</span>"
				       id (string= (nth 1 (cadr items)) current-uri) (if accesskey (format nil "~A [~A]" description accesskey) description) (and html (funcall html)) uri accesskey nofollow name)))))
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
  (let* ((username (logged-in-username)))
    (let ((*secondary-nav* `(("archive" "/archive" "Archive")
			     ("about" "/about" "About")
			     ("search" "/search" "Search" :html ,#'search-bar-to-html)
			     ,(if username
				`("login" ,(format nil "/users/~A" (encode-entities (get-user-slug (logged-in-userid)))) ,(plump:encode-entities username))
				`("login" ,(format nil "/login?return=~A" (url-rewrite:url-encode current-uri)) "Log In")))))
      (nav-bar-to-html current-uri)))) 

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

(defun begin-html (out-stream &key title description current-uri content-class robots)
  (let* ((session-token (hunchentoot:cookie-in "session-token"))
	 (csrf-token (and session-token (make-csrf-token session-token)))) 
    (format out-stream "<!DOCTYPE html><html lang=\"en-US\"><head><title>~@[~A - ~]LessWrong 2 viewer</title>~@[<meta name=\"description\" content=\"~A\">~]~A<link rel=\"stylesheet\" href=\"~A\"><link rel=\"stylesheet\" href=\"~A\"><style id='width-adjust'></style><link rel=\"shortcut icon\" href=\"~A\"><script src=\"~A\" async></script><script src=\"~A\" async></script><script>~A</script>~@[<script>var csrfToken=\"~A\"</script>~]~@[<meta name=\"robots\" content=\"~A\">~]</head><body><div id=\"content\"~@[ class=\"~A\"~]>~A"
	    title description
	    *html-head*
	    (generate-versioned-link (if (search "Windows" (hunchentoot:header-in* :user-agent)) "/style.windows.css" "/style.css"))
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

(defun end-html (out-stream &key next prev)
  (let ((request-uri (hunchentoot:request-uri*)))
    #|(format out-stream "<div id=\"bottom-bar\" class=\"nav-bar\">~@[<a class=\"nav-item nav-inner\" href=\"~A\">Previous</a>~]<a class=\"nav-item nav-inner\" href=\"#top\">Back to top</a>~@[<a class=\"nav-item nav-inner\" href=\"~A\">Next</a>~]</div></div></body></html>"
	      (if prev )
	      (if next ))|#
    (write-string
      (nav-bar-outer "bottom-bar" nil (nav-bar-inner
					`(,@(if (and prev (> prev 0)) `(("first" ,(replace-query-params request-uri "offset" nil) "Back to first")))
					  ,@(if prev `(("prev" ,(replace-query-params request-uri "offset" (if (= prev 0) nil prev)) "Previous" :nofollow t)))
					   ("top" "#top" "Back to top")
					   ,@(if next `(("next" ,(replace-query-params request-uri "offset" next) "Next" :nofollow t))))))
      out-stream)))

(defun map-output (out-stream fn list)
  (loop for item in list do (write-string (funcall fn item) out-stream))) 

(defmacro with-outputs ((out-stream) &body body) 
  (alexandria:with-gensyms (stream-sym) 
			   (let ((out-body (map 'list (lambda (x) `(princ ,x ,stream-sym)) body)))
			     `(let ((,stream-sym ,out-stream)) 
				,.out-body)))) 

(defmacro emit-page ((out-stream &key title description current-uri content-class (return-code 200) with-offset with-next robots) &body body)
  `(ignore-errors
     (log-conditions
       (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
	     (hunchentoot:return-code*) ,return-code)
       (let ((,out-stream (make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
	 (begin-html ,out-stream :title ,title :description ,description :current-uri ,current-uri :content-class ,content-class :robots ,robots)
	 ,@body
	 (end-html ,out-stream :next (if (and ,with-offset ,with-next) (+ ,with-offset 20)) :prev (if (and ,with-offset (>= ,with-offset 20)) (- ,with-offset 20)))))))

(defmacro with-error-page (&body body)
  `(let ((*current-auth-token* (hunchentoot:cookie-in "lw2-auth-token")))
     (handler-case
       (log-conditions 
	 (progn ,@body))
       (serious-condition (condition)
			  (emit-page (out-stream :title "Error" :return-code 500) 
				     (format out-stream "<h1>Error</h1><p>~A</p>"
					     condition)))))) 

(defun output-form (out-stream method action id class csrf-token fields button-label &key textarea)
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
  (format out-stream "<div><input type=\"hidden\" name=\"csrf-token\" value=\"~A\"><input type=\"submit\" value=\"~A\"></div></div></form>"
	  csrf-token button-label)) 

(defun view-items-index (items &key section with-offset (with-next t) title hide-title need-auth extra-html (content-class "index-page"))
  (alexandria:switch ((hunchentoot:get-parameter "format") :test #'string=)
                     ("rss" 
                      (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
                      (let ((out-stream (hunchentoot:send-headers)))
                        (write-index-items-to-rss (make-flexi-stream out-stream :external-format :utf-8) items :title title)))
                     (t
                       (emit-page (out-stream :title (if hide-title nil title) :description "A faster way to browse LessWrong 2.0" :content-class content-class :with-offset with-offset :with-next with-next
                                              :robots (if (and with-offset (> with-offset 0)) "noindex, nofollow"))
                                  (format out-stream "<div class=\"page-toolbar\">~@[<a class=\"new-post button\" href=\"/edit-post?section=~A\">New post</a>~]~{~@[<a class=\"rss\" rel=\"alternate\" type=\"application/rss+xml\" title=\"~A RSS feed\" href=\"~A\">RSS</a>~]~}</div>~@[~A~]"
                                          (if (and section (logged-in-userid)) section)
                                          (unless need-auth
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

(declaim (inline alist)) 
(defun alist (&rest parms) (alexandria:plist-alist parms))

(defparameter *posts-index-fields* '(:title :--id :slug :user-id :posted-at :base-score :comment-count :page-url :url))
(defparameter *comments-index-fields* '(:--id :user-id :post-id :posted-at :parent-comment-id :base-score :page-url :html-body)) 

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

(hunchentoot:define-easy-handler (view-feed :uri "/feed") (view meta before after)
				 (setf (hunchentoot:content-type*) "application/rss+xml; charset=utf-8")
				 (let ((posts (lw2-graphql-query (make-posts-list-query :view (or view "new") :meta (not (not meta)) :before before :after after)))
				       (out-stream (hunchentoot:send-headers)))
				   (write-index-items-to-rss (make-flexi-stream out-stream :external-format :utf-8) posts))) 

(hunchentoot:define-easy-handler (view-post-lw2-link :uri (lambda (r) (declare (ignore r)) (match-lw2-link (hunchentoot:request-uri*)))) ((csrf-token :request-type :post) (text :request-type :post) (parent-comment-id :request-type :post) (edit-comment-id :request-type :post) need-auth chrono)
				 (with-error-page
				   (cond
				     (text
				       (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
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
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (generate-post-link (match-lw2-link (hunchentoot:request-uri*)) new-comment-id)))))
				     (t 
				       (multiple-value-bind (post-id comment-id) (match-lw2-link (hunchentoot:request-uri*))
					 (if comment-id 
					   (setf (hunchentoot:return-code*) 303
						 (hunchentoot:header-out "Location") (generate-post-link post-id comment-id))
					   (let* ((lw2-auth-token (hunchentoot:cookie-in "lw2-auth-token"))
                                                  (post (get-post-body post-id :auth-token (and need-auth lw2-auth-token))))
					     (emit-page (out-stream :title (clean-text (cdr (assoc :title post))) :content-class "post-page") 
							(with-outputs (out-stream) (post-body-to-html post))
							(if lw2-auth-token
							  (format out-stream "<script>postVote=~A</script>~@[<div class=\"post-controls\"><a class=\"edit-post-link button\" href=\"/edit-post?post-id=~A\">Edit post</a></div>~]"
								  (json:encode-json-to-string (get-post-vote post-id lw2-auth-token))
								  (if (equal (logged-in-userid) (cdr (assoc :user-id post))) (cdr (assoc :--id post)))))
							(force-output out-stream) 
							(format out-stream "<div id=\"comments\">~A</div>"
								(let ((comments (get-post-comments post-id)))
								  (if chrono
								    (comment-chrono-to-html comments) 
								    (comment-tree-to-html (make-comment-parent-hash comments)))))
							(if lw2-auth-token
							  (format out-stream "<script>commentVotes=~A</script>" (json:encode-json-to-string (get-post-comments-votes post-id lw2-auth-token)))))))))))) 

(defparameter *edit-post-template* (compile-template* "edit-post.html"))

(hunchentoot:define-easy-handler (view-edit-post :uri "/edit-post") ((csrf-token :request-type :post) (text :request-type :post) title url section post-id link-post)
                                 (with-error-page
                                   (cond
                                     (text
                                       (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
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
                                      (let* ((csrf-token (make-csrf-token (hunchentoot:cookie-in "session-token")))
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
				 (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
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
                               (user-info (lw2-graphql-query (format nil "{UsersSingle(slug:\"~A\"){_id, displayName, karma}}" user-slug)))
                               (comments-index-fields (remove :page-url *comments-index-fields*)) ; page-url sometimes causes "Cannot read property '_id' of undefined" error
                               (title (format nil "~A~@['s ~A~]" (cdr (assoc :display-name user-info)) (if (member show '(nil "posts" "comments") :test #'equal) show)))
                               (items (alexandria:switch (show :test #'string=)
                                                         ("posts"
                                                          (lw2-graphql-query (graphql-query-string "PostsList" (alist :terms (alist :view "new" :limit 21 :offset offset :user-id (cdr (assoc :--id user-info)))) *posts-index-fields*)))
                                                         ("comments"
                                                          (lw2-graphql-query (graphql-query-string "CommentsList" (alist :terms (alist :view "postCommentsNew" :limit 21 :offset offset :user-id (cdr (assoc :--id user-info))))
                                                                                                   comments-index-fields)))
                                                         ("drafts"
                                                          (lw2-graphql-query (graphql-query-string "PostsList" (alist :terms (alist :view "drafts" :limit 21 :offset offset :user-id (cdr (assoc :--id user-info)))) *posts-index-fields*)
                                                                             :auth-token (hunchentoot:cookie-in "lw2-auth-token")))
                                                         (t
                                                           (let ((user-posts (lw2-graphql-query (graphql-query-string "PostsList" (alist :terms (alist :view "new" :limit (+ 21 offset) :user-id (cdr (assoc :--id user-info)))) *posts-index-fields*)))
                                                                 (user-comments (lw2-graphql-query (graphql-query-string "CommentsList" (alist :terms (alist :view "postCommentsNew" :limit (+ 21 offset) :user-id (cdr (assoc :--id user-info))))
                                                                                                                         comments-index-fields))))
                                                             (concatenate 'list user-posts user-comments)))))
                               (interleave (comment-post-interleave items :limit 20 :offset (if show nil offset))))
                          (view-items-index interleave :with-offset offset :title title :content-class "user-page"
                                            :with-offset offset :with-next (> (length items) (+ (if show 0 offset) 20))
                                            :need-auth (string= show "drafts") :section (if (string= show "drafts") "drafts" nil)
                                            :extra-html (format nil "<h1 class=\"page-main-heading\">~A</h1><div class=\"user-stats\">Karma: <span class=\"karma-total\">~A</span></div><div class=\"sublevel-nav\">~A</div>"
                                                                (cdr (assoc :display-name user-info))
                                                                (pretty-number (or (cdr (assoc :karma user-info)) 0))
                                                                (with-output-to-string (stream)
                                                                  (loop for (l-show text) in `((nil "All") ("posts" "Posts") ("comments" "Comments")
                                                                                               ,@(if (logged-in-userid (cdr (assoc :--id user-info))) '(("drafts" "Drafts"))))
                                                                        do (link-if-not stream (string= show l-show) (format nil "~A~@[?show=~A~]" (hunchentoot:script-name*) l-show)
                                                                                        "sublevel-item" text))))))))

(defun search-result-markdown-to-html (item)
  (cons (cons :html-body (markdown:parse (cdr (assoc :body item)))) item)) 

(hunchentoot:define-easy-handler (view-search :uri "/search") (q)
				 (with-error-page
				   (let ((*current-search-query* q)
					 (link (or (convert-lw2-link q) (convert-lw2-slug-link q) (convert-lw1-link q))))
				     (declare (special *current-search-query*))
				     (if link
				       (setf (hunchentoot:return-code*) 301
					     (hunchentoot:header-out "Location") link)
				       (multiple-value-bind (posts comments) (lw2-search-query q)
					 (emit-page (out-stream :title "Search" :current-uri "/search" :content-class "search-results-page")
						    (map-output out-stream #'post-headline-to-html posts)
						    (with-outputs (out-stream) "<ul class=\"comment-thread\">") 
						    (map-output out-stream (lambda (c) (format nil "<li class=\"comment-item\">~A</li>" (comment-to-html (search-result-markdown-to-html c) :with-post-title t))) comments)
						    (with-outputs (out-stream) "</ul>"))))))) 

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
								  '(("login-username" "Username" "text" "username")
								    ("login-password" "Password" "password" "current-password"))
								  "Log in")
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
					 (check-csrf-token (hunchentoot:cookie-in "session-token") csrf-token)
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
						    (hunchentoot:set-cookie "lw2-auth-token" :value auth-token :secure *secure-cookies* :max-age (- (expt 2 31) 1))
						    (cache-put "auth-token-to-userid" auth-token user-id)
						    (cache-put "auth-token-to-username" auth-token signup-username)
						    (setf (hunchentoot:return-code*) 303
							  (hunchentoot:header-out "Location") (if (and return (ppcre:scan "^/[~/]" return)) return "/"))))))))
				       (t
					 (emit-login-page)))))) 

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
                                              (emit-page (out-stream :title "Archive" :current-uri "/archive" :content-class "archive-page" :with-offset offset :with-next (> (length posts) 50))
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
                                                (map-output out-stream #'post-headline-to-html (firstn posts 50))))))))))

(hunchentoot:define-easy-handler (view-about :uri "/about") ()
				 (with-error-page
				   (emit-page (out-stream :title "About" :current-uri "/about" :content-class "about-page")
					      (alexandria:with-input-from-file (in-stream "www/about.html" :element-type '(unsigned-byte 8))
									       (alexandria:copy-stream in-stream out-stream))))) 

(defmacro define-versioned-resource (uri content-type)
  `(hunchentoot:define-easy-handler (,(alexandria:symbolicate "versioned-resource-" uri) :uri ,uri) (v)
				    (when v (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1)))) 
				    (hunchentoot:handle-static-file ,(concatenate 'string "www" uri) ,content-type))) 

(define-versioned-resource "/style.css" "text/css")
(define-versioned-resource "/style.windows.css" "text/css")
(define-versioned-resource "/style-dark.css" "text/css")
(define-versioned-resource "/style-dark.windows.css" "text/css")
(define-versioned-resource "/style-grey.css" "text/css")
(define-versioned-resource "/style-grey.windows.css" "text/css")
(define-versioned-resource "/style-ultramodern.css" "text/css")
(define-versioned-resource "/style-ultramodern.windows.css" "text/css")
(define-versioned-resource "/style-zero.css" "text/css")
(define-versioned-resource "/style-zero.windows.css" "text/css")
(define-versioned-resource "/style-brutalist.css" "text/css")
(define-versioned-resource "/style-brutalist.windows.css" "text/css")
(define-versioned-resource "/theme_tweaker.css" "text/css")
(define-versioned-resource "/script.js" "text/javascript") 
(define-versioned-resource "/guiedit.js" "text/javascript") 
(define-versioned-resource "/favicon.ico" "image/x-icon") 
(define-versioned-resource "/fa-regular-400.ttf" "application/x-font-ttf; charset=binary")
(define-versioned-resource "/fa-solid-900.ttf" "application/x-font-ttf; charset=binary")
(define-versioned-resource "/basilisk.png" "image/png")
(define-versioned-resource "minimize_button_icon.gif" "image/gif")
