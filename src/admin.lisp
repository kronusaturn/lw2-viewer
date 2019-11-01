(uiop:define-package #:lw2.admin
  (:use #:cl #:alexandria #:lw2.lmdb #:lw2.clean-html #:lw2.backend)
  (:export #:reclean-html))

(in-package #:lw2.admin)

(defun reclean-html ()
  (let ((total-count (count-database-entries "post-body-json"))
	(done-count 0)
	(last-done nil))
    (truncate-database "clean-html-memo")
    (format t "Press Enter to abort.~%")
    (labels ((report-progress ()
	       (when (= 0 (mod done-count 1))
		 (format t "Finished ~A of ~A posts.~A" done-count total-count (string #\Return))
		 (force-output))))
      (loop
	 for (post-json post-id) = (mapcar (lambda (x) (and x (flexi-streams:octets-to-string x :external-format :utf-8)))
					   (call-with-cursor "post-body-json"
							     (lambda (db cursor)
							       (declare (ignore db))
							       (multiple-value-list
								(if last-done
								    (progn
								      (lmdb:cursor-get cursor :set-range last-done)
								      (lmdb:cursor-get cursor :next))
								    (lmdb:cursor-get cursor :first))))))
	 while post-json
	 do (when (read-char-no-hang)
	      (format t "Aborted.~%")
	      (return-from reclean-html (values)))
	 do (report-progress)
	 do (let ((post (json:decode-json-from-string post-json)))
	      (ignore-errors (clean-html (or (cdr (assoc :html-body post)) "") :with-toc t :post-id post-id))
	      (ignore-errors
		(let ((comments (if (cdr (assoc :question post))
				    (append (get-post-comments post-id :revalidate nil)
					    (get-post-answers post-id :revalidate nil))
				    (get-post-comments post-id :revalidate nil))))
		  (loop for comment in comments
		     do (ignore-errors (clean-html (or (cdr (assoc :html-body comment)) "")))))))
	   (incf done-count)
	   (setf last-done post-id))
      (report-progress)
      (format t "~%Done.~%")
      (values))))