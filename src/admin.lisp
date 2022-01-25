(uiop:define-package #:lw2.admin
  (:use #:cl #:alexandria #:lw2.lmdb #:lw2.clean-html #:lw2.backend #:lw2.backlinks)
  (:export #:reclean-html))

(in-package #:lw2.admin)

(defun map-posts-and-comments (fn &key skip-comments)
  (let ((total-count (count-database-entries "post-body-json"))
	(done-count 0)
	(last-done nil))
    (format *error-output* "Press Enter to abort.~%")
    (labels ((report-progress ()
	       (when (= 0 (mod done-count 1))
		 (format *error-output* "Finished ~A of ~A posts.~A" done-count total-count (string #\Return))
		 (force-output *error-output*))))
      (loop
	 for (post post-id) = (call-with-cursor "post-body-json"
						(lambda (db cursor)
						  (declare (ignore db))
						  (multiple-value-bind (post post-id)
						      (if last-done
							  (progn
							    (cursor-get cursor :set-range :key last-done :return-type 'existence)
							    (cursor-get cursor :next :value-type :json))
							  (cursor-get cursor :first :value-type :json))
						    (list (ignore-errors (postprocess-query-result post)) post-id))))
	 while post-id
	 do (when (read-char-no-hang)
	      (format *error-output* "Aborted.~%")
	      (return-from map-posts-and-comments (values)))
	 do (report-progress)
	 do (progn
	      (with-simple-restart (continue "Ignore this post and continue.")
		(funcall fn post post-id))
	      (unless skip-comments
		(ignore-errors
		  (let ((comments (if (cdr (assoc :question post))
				      (append (get-post-comments post-id :revalidate nil)
					      (get-post-answers post-id :revalidate nil))
				      (get-post-comments post-id :revalidate nil))))
		    (loop for comment in comments
		       for comment-id = (cdr (assoc :--id comment))
		       do (with-simple-restart (continue "Ignore this comment and continue.")
			    (funcall fn comment post-id comment-id))))))
	      (incf done-count)
	      (setf last-done post-id)))
      (report-progress)
      (format *error-output* "~%Done.~%")
      (values))))

(defun reclean-html ()
  (map-posts-and-comments
   (lambda (item post-id &optional comment-id)
     (if (not comment-id)
	 (ignore-errors
	   (let ((*before-clean-hook* (lambda () (clear-backlinks post-id)))
		 (*link-hook* (lambda (link)
				(add-backlink link post-id))))
	     (clean-html (or (cdr (assoc :html-body item)) "") :with-toc t :post-id post-id)))
	 (ignore-errors
	   (let ((*before-clean-hook* (lambda () (clear-backlinks post-id comment-id)))
		 (*link-hook* (lambda (link)
				(add-backlink link post-id comment-id))))
	     (clean-html (or (cdr (assoc :html-body item)) ""))))))))

(defun grep-posts-and-comments (regex &key skip-comments print-ids)
  (let* ((scanner (ppcre:create-scanner regex))
	 (printer (if print-ids
		      (lambda (item post-id &optional comment-id)
			(when (ppcre:scan scanner (or (cdr (assoc :html-body item)) ""))
			  (format t "~A~@[/~A~]~%" post-id comment-id)))
		      (lambda (item post-id &optional comment-id)
			(declare (ignore post-id comment-id))
			(ppcre:do-matches-as-strings (match scanner (or (cdr (assoc :html-body item)) ""))
			  (write-line match))))))
    (map-posts-and-comments
     printer
     :skip-comments skip-comments)))
