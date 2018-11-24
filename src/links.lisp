(defpackage #:lw2.links
  (:use #:cl #:alexandria #:lw2.lmdb #:lw2.backend #:lw2.sites #:lw2.context #:lw2-viewer.config)
  (:export #:match-lw1-link #:convert-lw1-link
           #:match-overcomingbias-link #:convert-overcomingbias-link
           #:direct-link #:with-direct-link
           #:match-lw2-link #:match-lw2-slug-link #:match-lw2-sequence-link #:convert-lw2-link #:convert-lw2-slug-link #:convert-lw2-sequence-link #:convert-lw2-user-link
           #:generate-post-link
           #:convert-any-link))

(in-package #:lw2.links)

(defun get-redirect (uri)
  (multiple-value-bind (body status headers uri)
    (drakma:http-request uri :method :head :close t :redirect nil)
    (declare (ignore body uri))
    (let ((location (cdr (assoc :location headers))))
      (if (and (typep status 'integer) (< 300 status 400) location)
          location
          nil))))

(defun match-lw1-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(?:^https?://(?:www.)?less(?:er)?wrong.com|^)(?:/r/discussion|/r/lesswrong)?(/lw/.*)" link)
    (when match?
      (values (elt strings 0))))) 

(simple-cacheable ("lw1-link" "lw1-link" link :catch-errors nil)
  (if-let ((location (get-redirect (concatenate 'string "https://www.lesswrong.com" link))))
          (let ((loc-uri (puri:parse-uri location))) (format nil "~A~@[#comment-~A~]" (puri:uri-path loc-uri) (puri:uri-fragment loc-uri)))
          (error "<p>Could not retrieve LW1 link.</p><p>You may wish to try <a href='~A'>~:*~A</a>" (concatenate 'string "https://www.lesswrong.com" link))))

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

(defun convert-lw1-link (link)
  (if-let (matched-link (match-lw1-link link))
    (with-direct-link-restart ((concatenate 'string "https://www.lesswrong.com" matched-link))
      (get-lw1-link matched-link))))

(defun match-overcomingbias-link (link)
  (if (ppcre:scan "^https?://(?:www\\.)?overcomingbias\\.com/" link)
      link
      nil))

(simple-cacheable ("overcomingbias-link" "overcomingbias-link" link :catch-errors nil)
  (if-let ((location (get-redirect link)))
          (match-lw1-link location)
          ""))

(defun convert-overcomingbias-link (link)
  (with-direct-link-restart (link)
    (let ((lw1-link (get-overcomingbias-link link)))
      (if (string= lw1-link "")
          nil
          (convert-lw1-link lw1-link)))))

(defun match-lw2-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^(?:https?://(?:www.)?less(?:er)?wrong.com)?/posts/([^/]+)/([^/#]*)(?:/comment/([^/#]+)|/?#?([^/#]+)?)?" link)
    (when match?
      (values (elt strings 0) (or (elt strings 2) (elt strings 3)) (elt strings 1)))))

(defun match-lw2-slug-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^(?:https?://(?:www.)?less(?:er)?wrong.com)?/(?:codex|hpmor)/([^/#]+)(?:/?#?([^/#]+)?)?" link)
    (when match?
      (values (elt strings 0) (elt strings 1)))))

(defun match-lw2-sequence-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^(?:https?://(?:www.)?less(?:er)?wrong.com)?/s/(?:[^/#]+)/p/([^/#]+)(?:#([^/#]+)?)?" link)
    (when match?
      (values (elt strings 0) (elt strings 1)))))

(defun convert-lw2-user-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^(?:https?://(?:www.)?less(?:er)?wrong.com)(/users/[^/#]+)" link)
    (when match?
      (elt strings 0))))

(labels
  ((gen-internal (post-id slug comment-id &optional absolute-uri)
		 (format nil "~Aposts/~A/~A~@[#comment-~A~]" (if absolute-uri (site-uri *current-site*) "/") post-id (or slug "-") comment-id))) 

  (defun convert-lw2-slug-link (link)
    (multiple-value-bind (slug comment-id) (match-lw2-slug-link link)
      (when slug
        (gen-internal (get-slug-postid slug) slug comment-id))))

  (defun convert-lw2-sequence-link (link)
    (multiple-value-bind (post-id comment-id) (match-lw2-sequence-link link)
      (when post-id
        (gen-internal post-id (get-post-slug post-id) comment-id))))

  (defun convert-lw2-link (link)
    (multiple-value-bind (post-id comment-id slug) (match-lw2-link link)
      (when post-id 
	(gen-internal post-id slug comment-id)))) 

  (defun generate-post-link (story &optional comment-id absolute-uri) 
    (typecase story
      (string 
	(gen-internal story (get-post-slug story) comment-id absolute-uri))
      (cons
	(let ((story-id (cdr (assoc :--id story)))) 
	  (gen-internal story-id (or (cdr (assoc :slug story)) (get-post-slug story-id)) comment-id absolute-uri))))))

(defun convert-any-link (url)
  (or (convert-lw2-link url) (convert-lw2-slug-link url) (convert-lw2-sequence-link url) (convert-lw1-link url) (convert-overcomingbias-link url) (convert-lw2-user-link url)))
