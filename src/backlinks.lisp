(uiop:define-package #:lw2.backlinks
  (:use #:cl #:alexandria #:split-sequence
	#:lw2.html-reader #:lw2.lmdb #:lw2.backend-modules #:lw2.backend #:lw2.sites #:lw2.links #:lw2.context #:lw2.clean-html #:lw2.conditions #:lw2.interface-utils)
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
	(loop for (site-host post comment)
	   in (sort
	       (mapcar (lambda (bl)
			 (destructuring-bind (&optional site-host post-id comment-id) bl
			   (or
			    (log-and-ignore-errors
			     (when site-host
			       (with-site-context ((find-site site-host))
				 (list site-host
				       (get-post-body post-id :revalidate nil)
				       (when comment-id
					 (find-if (lambda (c) (string= comment-id (cdr (assoc :--id c))))
						  (get-post-comments post-id)))))))
			    (list nil nil nil))))
		       backlinks)
	       (lambda (x y) (> (or x 0) (or y 0)))
	       :key (lambda (bl)
		      (destructuring-bind (&optional site-host post comment) bl
			(declare (ignore site-host))
			(cdr (assoc :base-score (or comment post))))))
	   do
	     (log-and-ignore-errors
	      (when site-host
		(with-site-context ((find-site site-host))
		  <li>
		    <a href=(generate-post-link post (cdr (assoc :--id comment)) t)>
		      (with-html-stream-output
			  (when comment
			    <span class="author">(get-username (cdr (assoc :user-id comment)))</span>
			    (format *html-output* "'s comment on ")))
		      (safe (clean-text-to-html (cdr (assoc :title post))))
		      (" (")
		      (when (not (eq *current-site* original-site))
			(format *html-output* "~A; " (main-site-title *current-site*)))
		      (multiple-value-bind (pretty-time js-time) (pretty-time (cdr (assoc :posted-at (or comment post))))
			<span class="date" data-js-date=js-time>(progn pretty-time)</span>)
		      ("; ~A point~:*~P)" (cdr (assoc :base-score (or comment post))))
		    </a>
		  </li>)))))
      </ul>
    </div>))
