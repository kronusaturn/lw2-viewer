(defpackage #:lw2.links
  (:use #:cl #:alexandria #:lw2.lmdb #:lw2.backend #:lw2.sites #:lw2.context #:lw2-viewer.config)
  (:export #:match-lw1-link #:convert-lw1-link
	   #:match-ea1-link #:convert-ea1-link
           #:match-overcomingbias-link #:convert-overcomingbias-link
           #:direct-link #:with-direct-link
           #:match-lw2-link #:match-lw2-slug-link #:match-lw2-sequence-link #:convert-lw2-link #:convert-lw2-slug-link #:convert-lw2-sequence-link #:convert-lw2-user-link
           #:generate-post-link
           #:convert-any-link* #:convert-any-link))

(in-package #:lw2.links)

(defun sanitize-link (link)
  (substitute #\+ #\Space (string-trim '(#\Space) link)))

(defun get-redirect (uri)
  (multiple-value-bind (body status headers uri)
    (drakma:http-request uri :method :head :close t :redirect nil)
    (declare (ignore body uri))
    (let ((location (cdr (assoc :location headers))))
      (if (and (typep status 'integer) (< 300 status 400) location)
          location
          nil))))

(defmacro match-values (regex input registers)
  (with-gensyms (match? strings)
    (labels ((register-body (x)
               (typecase x
                 (integer `(elt ,strings ,x))
                 (atom x)
                 (t (cons (register-body (car x)) (register-body (cdr x)))))))
      `(multiple-value-bind (,match? ,strings) (ppcre:scan-to-strings ,regex ,input)
         (when ,match?
           (values ,.(register-body registers)))))))

(defmethod link-for-site-p ((s site) link) nil)

(defmethod link-for-site-p ((s lesswrong-viewer-site) link)
  (ppcre:scan "^https?://(?:www\\.)?(?:less(?:er|est)?wrong\\.com|alignmentforum\\.org)" link))

(defmethod link-for-site-p ((s ea-forum-viewer-site) link)
  (ppcre:scan "^https?://(?:www\\.)?(?:effective-altruism\\.com|forum\\.effectivealtruism\\.org)" link))

(defmethod link-for-site-p ((s arbital-site) link)
  (ppcre:scan "^https?://(?:www\\.)?(?:arbital\\.com)" link))

(defun find-link-site (link)
  (if (ppcre:scan "^/" link)
      *current-site*
      (loop for s in *sites*
	 when (link-for-site-p s link) return s)))

(defun site-link-prefix (site)
  (if (eq site *current-site*)
      "/"
      (site-uri site)))

(defun match-lw1-link (link) (match-values "(?:^https?://(?:www.)?less(?:er|est)?wrong.com|^)(?:/r/discussion|/r/lesswrong)?(/lw/.*)" link (0)))

(defun match-ea1-link (link) (match-values "^(?:https?://(?:www\\.)?(?:effective-altruism\\.com|forum\\.effectivealtruism\\.org))?(/ea/.*)" link (0)))

(defun match-agentfoundations-link (link) (match-values "^(?:https?://(?:www\\.)?agentfoundations\\.org)?(/item\\?id=.*)" link (0)))

(defun match-lw2-link (link) (match-values "^(?:https?://[^/]+)?/posts/([^/]+)/([^/#?]*)(?:/(comment|answer)/([^/#?]+)|/?(?:#(?:comment-)?|\\?commentId=)([^/#]+))?" link (0 (or 3 4) 1 2)))

(defun match-lw2-slug-link (link) (match-values "^(?:https?://(?:www.)?less(?:er|est)?wrong.com)?/(?:codex|hpmor)/([^/#]+)(?:/?#?([^/#]+)?)?" link (0 1)))

(defun match-lw2-sequence-link (link) (match-values "^(?:https?://[^/]+)?/s/([^/#]+)(?:/p/([^/#]+))?(?:#([^/#]+)?)?" link (0 1 2)))

(defun convert-lw2-user-link (link)
  (when-let ((site (find-link-site link))
	     (matched-link (match-values "^(?:https?://[^/]+)?/(users/[^/#]+)" link (0))))
    (concatenate 'string (site-link-prefix site) matched-link)))

(defun convert-arbital-link (link)
  (when-let ((site (find-link-site link))
	     (matched-link (match-values "^(?:https?://[^/]+)?/(.*)" link (0))))
    (concatenate 'string (site-link-prefix site) matched-link)))

(defmacro with-direct-link-restart ((direct-link) &body body)
  (once-only (direct-link)
    `(restart-case (progn ,@body)
       (direct-link () :report "Use direct link." ,direct-link))))

(defun direct-link (&optional c)
  (declare (ignore c))
  (if-let (restart (find-restart 'direct-link))
    (invoke-restart restart)))

(defmacro with-direct-link (&body body)
  `(handler-bind
     ((serious-condition #'direct-link))
     (progn ,@body)))

(defun process-redirect-link (link base-uri site-name)
  (if-let ((location (get-redirect (concatenate 'string base-uri link))))
          (let ((loc-uri (puri:parse-uri location))) (format nil "~A~@[#comment-~A~]" (puri:uri-path loc-uri) (puri:uri-fragment loc-uri)))
          (error "<p>Could not retrieve ~A link.</p><p>You may wish to try <a href='~A'>~:*~A</a>" site-name (concatenate 'string base-uri link))))

(defun convert-redirect-link (link match-fn get-fn base-uri)
  (if-let (matched-link (funcall match-fn link))
    (with-direct-link-restart ((concatenate 'string base-uri matched-link))
      (quri:render-uri
        (quri:merge-uris (quri:uri (funcall get-fn matched-link))
                         (quri:uri (site-uri (find-link-site base-uri))))))))

(simple-cacheable ("lw1-link" 'backend-lmdb-cache "lw1-link" link :catch-errors nil)
  (process-redirect-link link "https://www.lesswrong.com" "LessWrong 1.0"))

(defun convert-lw1-link (link)
  (convert-redirect-link link #'match-lw1-link #'get-lw1-link "https://www.lesswrong.com"))

(simple-cacheable ("ea1-link" 'backend-lmdb-cache "ea1-link" link :catch-errors nil)
  (process-redirect-link link "https://forum.effectivealtruism.org" "EA Forum 1.0"))

(defun convert-ea1-link (link)
  (convert-redirect-link link #'match-ea1-link #'get-ea1-link "https://forum.effectivealtruism.org"))

(defun match-overcomingbias-link (link)
  (if (ppcre:scan "^https?://(?:www\\.)?overcomingbias\\.com/" link)
      link
      nil))

(simple-cacheable ("overcomingbias-link" 'backend-lmdb-cache "overcomingbias-link" link :catch-errors nil)
  (if-let ((location (get-redirect link)))
          (match-lw1-link location)
          ""))

(defun convert-overcomingbias-link (link)
  (when (match-overcomingbias-link link)
    (with-direct-link-restart (link)
      (let ((lw1-link (get-overcomingbias-link link)))
        (if (string= lw1-link "")
            nil
            (convert-lw1-link lw1-link))))))

(simple-cacheable ("agentfoundations-link" 'backend-lmdb-cache "agentfoundations-link" link :catch-errors nil)
  (process-redirect-link link "https://www.lesswrong.com" "Agent Foundations"))

(defun convert-agentfoundations-link (link)
  (convert-redirect-link link #'match-agentfoundations-link #'get-agentfoundations-link "https://www.lesswrong.com"))

(defun gen-internal (post-id slug comment-id &optional absolute-uri)
  (format nil "~Aposts/~A/~A~:[~@[#~A~]~;~@[#comment-~A~]~]" (or absolute-uri "/") post-id (or slug "-") (and comment-id (= (length comment-id) 17)) comment-id))

(defun convert-lw2-slug-link (link)
  (multiple-value-bind (slug comment-id) (match-lw2-slug-link link)
    (when slug
      (gen-internal (get-slug-postid slug) slug comment-id))))

(defun convert-lw2-sequence-link (link)
  (if-let (site (find-link-site link))
	  (multiple-value-bind (sequence-id post-id comment-id) (match-lw2-sequence-link link)
	    (cond
	      (post-id (gen-internal post-id (get-post-slug post-id) comment-id (site-link-prefix site)))
	      (sequence-id (format nil "~As/~A" (site-link-prefix site) sequence-id))))))

(defun convert-lw2-link (link)
  (multiple-value-bind (post-id comment-id slug) (match-lw2-link link)
    (when post-id
      (if-let (site (find-link-site link))
              (gen-internal post-id slug comment-id (site-link-prefix site))))))

(defun generate-post-link (story &optional comment-id absolute-uri)
  (let ((absolute-uri (if (eq absolute-uri t) (site-uri *current-site*) absolute-uri)))
    (typecase story
      (string 
        (gen-internal story (get-post-slug story) comment-id absolute-uri))
      (cons
        (let ((story-id (cdr (assoc :--id story)))) 
          (gen-internal story-id (or (cdr (assoc :slug story)) (get-post-slug story-id)) comment-id absolute-uri))))))

(defun convert-any-link* (url)
  (let ((url (sanitize-link url)))
    (or (convert-lw2-link url)
	(convert-lw2-slug-link url)
	(convert-lw2-sequence-link url)
	(convert-lw1-link url)
	(convert-ea1-link url)
	(convert-agentfoundations-link url)
	(convert-overcomingbias-link url)
	(convert-lw2-user-link url)
	(convert-arbital-link url))))

(defun convert-any-link (url)
  (or (convert-any-link* url) url))
