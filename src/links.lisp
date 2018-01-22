(defpackage #:lw2.links
  (:use #:cl #:lw2.lmdb #:lw2.backend #:lw2-viewer.config)
  (:export #:match-lw1-link #:convert-lw1-link #:match-lw2-link #:convert-lw2-link #:generate-post-link))

(in-package #:lw2.links)

(defun match-lw1-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(?:^https?://(?:www.)?less(?:er)?wrong.com|^)(?:/r/discussion|/r/lesswrong)?(/lw/.*)" link)
    (when match?
      (values (elt strings 0))))) 

(simple-cacheable ("lw1-link" "lw1-link" link :catch-errors nil)
  (multiple-value-bind (body status headers uri)
    (ignore-errors (drakma:http-request (concatenate 'string "https://www.lesserwrong.com" link) :method :head :close t))
    (declare (ignore body headers)) 
    (if (and (typep status 'integer) (< 300 status 400))
      (format nil "~A~@[#~A~]" (puri:uri-path uri) (puri:uri-fragment uri))
      (error "<p>Could not retrieve LW1 link.</p><p>You may wish to try <a href='~A'>~:*~A</a>" (concatenate 'string "http://lesswrong.com" link))))) 

(defun convert-lw1-link (link &key (if-error :signal))
  (alexandria:if-let (canonical-link (match-lw1-link link))
		     (ecase if-error
		       (:direct-link (or (ignore-errors (get-lw1-link canonical-link)) (concatenate 'string "http://lesswrong.com" canonical-link))) 
		       (:signal (get-lw1-link canonical-link))))) 

(defun match-lw2-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(^https?://(www.)?lesserwrong.com|^)/posts/([^/]+)/([^/]*)(/$|/([^/#]+))?(#|$)" link)
    (when match?
      (values (elt strings 2) (elt strings 5) (elt strings 3))))) 

(labels
  ((gen-internal (post-id slug comment-id &optional absolute-uri)
		 (format nil "~Aposts/~A/~A~@[#comment-~A~]" (if absolute-uri *site-uri* "/") post-id (or slug "-") comment-id))) 

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

