(uiop:define-package #:lw2.backlinks
  (:use #:cl #:alexandria #:split-sequence
	#:lw2.html-reader #:lw2.lmdb #:lw2.backend-modules #:lw2.backend #:lw2.sites #:lw2.links #:lw2.context #:lw2.clean-html #:lw2.conditions #:lw2.utils #:lw2.interface-utils)
  (:import-from #:collectors #:with-collector)
  (:export #:clear-backlinks #:add-backlink #:get-backlinks #:backlinks-to-html))

(in-package #:lw2.backlinks)

(named-readtables:in-readtable html-reader)

(define-cache-database 'backend-backlinks
  (list "backlinks" :flags liblmdb:+dupsort+)
  (list "frontlinks" :flags liblmdb:+dupsort+)
  "backlinks-cache")

(declaim (ftype (function (string (or null string) &optional string) string) item-reference-string))

(defun item-reference-string (post-id comment-id &optional host)
  (format nil "~@[~A ~]~A~@[ ~A~]" host post-id comment-id))

(define-backend-function clear-backlinks (post-id &optional comment-id)
  (backend-backlinks
   (cache-del "frontlinks" (item-reference-string post-id comment-id))))

(define-backend-function add-backlink (link post-id &optional comment-id)
  (backend-backlinks
   (let* ((link-host (or (quri:uri-host (quri:uri link)) (site-host *current-site*)))
	  (link-site (and link-host (find-site link-host)))
	  (current-host (site-host *current-site*)))
     (when link-site
       (multiple-value-bind (link-post-id link-comment-id) (match-lw2-link link)
	 (when link-post-id
	   (ignore-errors
	     (cache-put "frontlinks"
			(item-reference-string post-id comment-id)
			(item-reference-string link-post-id link-comment-id link-host))
	     (with-site-context (link-site)
	       (cache-put "backlinks"
			  (item-reference-string link-post-id link-comment-id)
			  (item-reference-string post-id comment-id current-host)))))))))
  (backend-base
   (declare (ignore link post-id comment-id))
   nil))

(define-backend-function link-exists-p (source-post-id source-comment-id target-host target-post-id target-comment-id)
  (backend-backlinks
   (call-with-cursor "frontlinks"
		     (lambda (db cursor)
		       (declare (ignore db))
		       (cursor-get cursor :get-both
				   :key (item-reference-string source-post-id source-comment-id)
				   :value (item-reference-string target-post-id target-comment-id target-host)
				   :return-type 'existence)))))

(define-backend-function get-backlink-pointers (post-id &optional comment-id)
  (backend-backlinks
   (call-with-cursor "backlinks"
		     (lambda (db cursor)
		       (declare (ignore db))
		       (loop for backlink-data = (cursor-get cursor :set :key (item-reference-string post-id comment-id))
			  then (cursor-get cursor :next-dup)
			  while backlink-data
			  collect (split-sequence #\Space backlink-data)))
		     :read-only t))
  (backend-base
   (declare (ignore post-id comment-id))
   nil))

(define-backend-function process-backlink (current-post-id current-comment-id source-site-host source-post-id &optional source-comment-id)
  (backend-backlinks
   (let* ((source-db (if source-comment-id "post-comments-json-meta" "post-body-json-meta"))
	  (metadata (cache-get source-db source-post-id :value-type :lisp))
	  (cache-key (format nil "~@{~S~^ ~}" current-post-id current-comment-id source-site-host source-post-id source-comment-id))
	  (cached-data (cache-get "backlinks-cache" cache-key :value-type :lisp))
	  (last-modified (cdr (assoc :last-modified metadata)))
	  (if-modified-since (cdr (assoc :if-modified-since cached-data))))
     (if (and last-modified if-modified-since (= last-modified if-modified-since))
	 cached-data
	 (log-and-ignore-errors
	  (let ((current-site-host (site-host *current-site*)))
	    (labels ((cleanup-stale-backlink ()
		       (with-cache-transaction
			   (cache-del "backlinks-cache" cache-key)
			 (cache-del "backlinks"
				    (item-reference-string current-post-id current-comment-id)
				    :value (item-reference-string source-post-id source-comment-id source-site-host)))
		       nil))
	      (handler-case
		  (with-site-context ((find-site source-site-host))
		    (if (not (link-exists-p source-post-id source-comment-id current-site-host current-post-id current-comment-id))
			(cleanup-stale-backlink)
			(let* ((source-post (get-post-body source-post-id :revalidate nil))
			       (source-comment (when source-comment-id
						 (find-if (lambda (c) (string= source-comment-id (cdr (assoc :--id c))))
							  (get-post-comments source-post-id :revalidate nil))))
			       (result
				(alist :if-modified-since last-modified
				       :site-host source-site-host
				       :link (generate-item-link :post source-post :comment-id source-comment-id :absolute t)
				       :post-title (cdr (assoc :title source-post))
				       :post-user-id (cdr (assoc :user-id source-post))
				       :comment-user-id (cdr (assoc :user-id source-comment))
				       :posted-at (cdr (assoc :posted-at (or source-comment source-post)))
				       :score (cdr (assoc :base-score (or source-comment source-post))))))
			  (cache-put "backlinks-cache" cache-key (prin1-to-string result))
			  result)))
		(lw2-client-error ()
		  (cleanup-stale-backlink))))))))))

(define-backend-function get-backlinks (post-id &optional comment-id)
  (backend-backlinks
   (loop
      for bp in (get-backlink-pointers post-id comment-id)
      for backlink-data = (apply 'process-backlink post-id comment-id bp)
      when backlink-data collect backlink-data))
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
	(loop for backlink-alist in (sort backlinks
					  (lambda (x y) (> (or x 0) (or y 0)))
					  :key (lambda (bl) (cdr (assoc :score bl))))
	   do
	     (log-and-ignore-errors
	      (alist-bind (site-host link post-title post-user-id comment-user-id posted-at score) backlink-alist
		(with-site-context ((find-site site-host))
		  <li>
		    <a href=link>
		      (with-html-stream-output
			  (when comment-user-id
			    <span class="inline-author" data-userid=comment-user-id>(get-username comment-user-id)</span>
			    (format *html-output* "'s comment on ")))
		      (safe (clean-text-to-html post-title))
		      (" by ")
		      <span class="inline-author" data-userid=post-user-id>(get-username post-user-id)</span>
		      (" (")
		      (when (not (eq *current-site* original-site))
			(format *html-output* "~A; " (main-site-title *current-site*)))
		      (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
			<span class="date" data-js-date=js-time>
			  (safe pretty-time)
			  (safe (pretty-time-js))
			</span>)
		      ("; ~A point~:*~P)" score)
		    </a>
		  </li>)))))
      </ul>
    </div>))
