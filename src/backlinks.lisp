(uiop:define-package #:lw2.backlinks
  (:use #:cl #:alexandria #:split-sequence
	#:lw2.html-reader #:lw2.lmdb #:lw2.backend-modules #:lw2.backend #:lw2.sites #:lw2.links #:lw2.context #:lw2.clean-html #:lw2.conditions)
  (:export #:add-backlink #:get-backlinks #:backlinks-to-html))

(in-package #:lw2.backlinks)

(named-readtables:in-readtable html-reader)

(define-cache-database 'backend-backlinks
    (list "backlinks" :flags liblmdb:+dupsort+))

(define-backend-function add-backlink (link post-id &optional comment-id)
  (backend-backlinks
   (let* ((link-host (or (quri:uri-host (quri:uri link)) (site-host *current-site*)))
	  (link-site (and link-host (find-site link-host)))
	  (current-host (site-host *current-site*)))
     (when link-site
       (multiple-value-bind (link-post-id link-comment-id) (match-lw2-link link)
	 (when link-post-id
	   (ignore-errors
	     (with-site-context (link-site)
	       (cache-put "backlinks"
			  (format nil "~A~@[ ~A~]" link-post-id link-comment-id)
			  (format nil "~A ~A~@[ ~A~]" current-host post-id comment-id)))))))))
  (backend-base
   (declare (ignore link post-id comment-id))
   nil))

(define-backend-function get-backlinks (post-id &optional comment-id)
  (backend-backlinks
   (call-with-cursor "backlinks"
		     (lambda (db cursor)
		       (declare (ignore db))
		       (loop for backlink-data = (cursor-get cursor :set (format nil "~A~@[ ~A~]" post-id comment-id))
			  then (cursor-get cursor :next-dup)
			  while backlink-data
			  collect (split-sequence #\Space backlink-data)))
		     :read-only t))
  (backend-base
   (declare (ignore post-id comment-id))
   nil))

(defun backlinks-to-html (backlinks id)
  (when backlinks
    <div class="backlinks">
      <input type="checkbox" id=("expand-~A" id)>
      <label for=("expand-~A" id)>What links here?</label>
      <ul>
      (let ((original-site *current-site*))
	(loop for (site-host post-id comment-id) in backlinks do
	     (log-and-ignore-errors
	      (with-site-context ((find-site site-host))
		(let* ((post (get-post-body post-id :revalidate nil))
		       (comment (when comment-id
				  (find-if (lambda (c) (string= comment-id (cdr (assoc :--id c))))
					   (get-post-comments post-id))))
		       (title (clean-text-to-html (cdr (assoc :title post)))))
		  <li>
		    <a href=(generate-post-link post comment-id t)>
		    (safe
		     (format nil "~@[~A's comment on ~]~A~@[ (~A)~]"
			     (if comment (get-username (cdr (assoc :user-id comment))))
			     title
			     (if (not (eq *current-site* original-site)) (main-site-title *current-site*))))
		    </a>
		  </li>)))))
      </ul>
    </div>))
