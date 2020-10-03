(in-package #:lw2.backend)

(defvar *background-loader-thread* nil)
(defvar *background-loader-semaphore* (make-semaphore :count 1))
(defvar *background-loader-ready* nil)

(defun background-loader-running-p ()
  (case (semaphore-count *background-loader-semaphore*)
    (0 t)
    (1 nil)))

(defun background-loader-ready-p ()
  (and (background-loader-running-p)
       (background-loader-enabled *current-site*)
       *background-loader-ready*))

(defun make-site-background-loader-fn (site)
  (let (last-comment-processed)
    (lambda ()
      (with-site-context (site)
	(log-and-ignore-errors
	 (let* ((posts-json (sb-sys:with-deadline (:seconds 120) (get-posts-json)))
		(posts-list (decode-query-result posts-json)))
	   (when posts-list
	     (with-cache-transaction
	       (cache-put "index-json" "new-not-meta" posts-json)
	       (dolist (post posts-list)
		 (cache-put "postid-to-title" (cdr (assoc :--id post)) (cdr (assoc :title post))))
	       (dolist (post posts-list)
		 (cache-put "postid-to-slug" (cdr (assoc :--id post)) (cdr (assoc :slug post))))))))
	(log-and-ignore-errors
	 (let ((recent-comments-json (sb-sys:with-deadline (:seconds 120) (get-recent-comments-json))))
	   (when-let (recent-comments (ignore-errors (decode-query-result recent-comments-json)))
		     (cache-put "index-json" "recent-comments" recent-comments-json)
		     (loop for comment in recent-comments
			as comment-id = (cdr (assoc :--id comment))
			as cache-database = (if (or (cdr (assoc :answer comment)) (cdr (assoc :parent-answer-id comment)))
						"post-answers-json"
						"post-comments-json")
			if (string= comment-id last-comment-processed) return nil
			do
			  (log-and-ignore-errors
			   (with-cache-transaction
			     (when-let ((post-id (cdr (assoc :post-id comment))))
			       (let* ((post-comments (when-let ((x (cache-get cache-database post-id :return-type 'binary-stream))) (decode-query-result x)))
				      (new-post-comments (sort (cons comment (delete-if (lambda (c) (string= comment-id (cdr (assoc :--id c)))) post-comments))
							       #'> :key (lambda (c) (cdr (assoc :base-score c))))))
				 (cache-update cache-database post-id (comments-list-to-graphql-json new-post-comments))))
			     (when-let ((user-id (cdr (assoc :user-id comment))))
				 (cache-mark-stale "user-page-items" user-id))))
			  (setf last-comment-processed (cdr (assoc :--id (first recent-comments))))))))
	(send-all-notifications)))))

(defun background-loader ()
  (let (sites loader-functions)
    (loop
       (unless (eq sites *sites*)
	 (setf sites *sites*
	       loader-functions (loop for site in sites
				   when (background-loader-enabled site)
				   collect (make-site-background-loader-fn site))))
       (dolist (loader-fn loader-functions)
	 (funcall loader-fn))
       (setf *background-loader-ready* t)
       (if (wait-on-semaphore *background-loader-semaphore* :timeout 60)
	   (return)))))

(defun start-background-loader ()
  (if (background-loader-running-p)
      (warn "Background loader already running.")
      (progn
        (wait-on-semaphore *background-loader-semaphore*)
        (setf *background-loader-thread* (sb-thread:make-thread #'background-loader :name "background loader")))))

(defun stop-background-loader ()
  (if (background-loader-running-p)
      (progn
        (signal-semaphore *background-loader-semaphore*)
        (join-thread *background-loader-thread*)
        (setf *background-loader-thread* nil
	      *background-loader-ready* nil)
        (signal-semaphore *background-loader-semaphore*))
      (warn "Background loader not running.")))
