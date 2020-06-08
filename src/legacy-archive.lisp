(uiop:define-package #:lw2.legacy-archive
  (:use #:cl #:lw2.utils #:lw2.backend)
  (:import-from #:cl-ppcre #:regex-replace-all)
  (:export #:lw-legacy-url))

(in-package #:lw2.legacy-archive)

;; Should match the behavior of https://github.com/tricycle/lesswrong/blob/925eb95151c6aaf1e97efe630a25516af493f9e4/r2/r2/lib/utils/utils.py#L1011
(defun lw-legacy-slug (title)
  (let* ((max-length 50)
	 (title (regex-replace-all "\\s+" title "_"))
	 (title (regex-replace-all "\\W+" title ""))
	 (title (regex-replace-all "_+" title "_"))
	 (title (string-trim "_" title))
	 (title (string-downcase title)))
    (if (> (length title) max-length)
	(substring title 0 (or (position #\_ title :end max-length :from-end t)
			       max-length))
	title)))

(defun lw-legacy-id-string (legacy-id)
  (format nil "~(~36R~)"
	  (etypecase legacy-id
	    (string (parse-integer legacy-id))
	    (integer legacy-id))))

(defun lw-legacy-url (legacy-id title &key (section :main))
  (format nil "~Alw/~A/~A"
	  (case section
	    (:main "http://lesswrong.com/")
	    (:discussion "http://lesswrong.com/r/discussion/")
	    (t ""))
	  (lw-legacy-id-string legacy-id)
	  (lw-legacy-slug title)))

(defun check-wayback-availability (url)
  (let* ((wayback-api-url
	  (quri:make-uri :defaults "https://archive.org/wayback/available"
			 :query (alist "url" url
				       "timestamp" "2009")))
	 (timestamp
	  (trivia:match
	     (call-with-http-response #'json:decode-json wayback-api-url :want-stream t :force-string t)
	   ((assoc :archived--snapshots
		   (assoc :closest
			  (assoc :timestamp timestamp)))
	    timestamp))))
    (and timestamp
	 (nth-value 0 (parse-integer timestamp)))))
