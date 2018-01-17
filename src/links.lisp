(defpackage #:lw2.links
  (:use #:cl #:lw2.lmdb #:lw2.backend #:lw2-viewer.config)
  (:export #:match-lw1-link #:convert-lw1-link #:match-lw2-link #:convert-lw2-link #:generate-post-link))

(in-package #:lw2.links)

(defun match-lw1-link (link)
  (multiple-value-bind (match? strings) (ppcre:scan-to-strings "(?:^https?://(?:www.)?less(?:er)?wrong.com|^)(?:/r/discussion)?(/lw/.*)" link)
    (when match?
      (values (elt strings 0))))) 

(simple-cacheable ("lw1-link" "lw1-link" link)
  (let ((out (nth-value 3 (drakma:http-request (concatenate 'string "https://www.lesserwrong.com" link) :method :head :close t))))
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
	(let ((story-id (cdr (assoc :--id story)))) 
	  (gen-internal story-id (or (cdr (assoc :slug story)) (get-post-slug story-id)) comment-id absolute-uri))))))

