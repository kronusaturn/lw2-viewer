(uiop:define-package #:lw2.interface-utils
  (:use #:cl #:lw2.links #:lw2.html-reader)
  (:import-from #:lw2.utils #:hash-cond #:alist-bind)
  (:export #:pretty-time #:pretty-time-js #:pretty-time-html
	   #:pretty-number #:generate-post-auth-link #:clean-lw-link #:votes-to-tooltip #:vote-buttons))

(in-package #:lw2.interface-utils)

(named-readtables:in-readtable html-reader)

(defun ensure-timestamp (timespec &optional loose-parsing)
  (etypecase timespec
    (local-time:timestamp timespec)
    (string (if loose-parsing
		(chronicity:parse timespec)
		(local-time:parse-timestring timespec)))))

(defun pretty-time (timespec &key format loose-parsing)
  (let ((time (ensure-timestamp timespec loose-parsing)))
    (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	    (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-time-js ()
  "<script async src='data:text/javascript,prettyDate()'></script>")

(defun pretty-time-html (timespec)
  (multiple-value-bind (pretty-time js-time) (pretty-time timespec)
    (format *html-output* "<span class=\"date hide-until-init\" data-js-date=~A>~A~A</span>"
	    js-time
	    pretty-time
	    (pretty-time-js))))

(defun pretty-number (number &optional object (output-format :html))
  (with-output-to-string (*standard-output*)
    (when (minusp number)
      (write-char #\MINUS_SIGN))
    (format t "~:D" (abs number))
    (when object
      (flet ((write-object () (format t " ~A~P" object number)))
	(cond ((eq output-format :html)
	       (write-string "<span>")
	       (write-object)
	       (write-string "</span>"))
	      (t (write-object)))))))

(defun maybe-need-auth (link need-auth)
  (if need-auth
      (concatenate 'string link "?need-auth=y")
      link))

(define-compiler-macro generate-post-auth-link (post &rest args &key need-auth &allow-other-keys)
  `(maybe-need-auth (generate-item-link :post ,post ,@(alexandria:remove-from-plist args :need-auth)) ,need-auth))

(defun generate-post-auth-link (post &rest args &key need-auth &allow-other-keys)
  (maybe-need-auth (apply #'generate-item-link :post post :allow-other-keys t args) need-auth))

(defun clean-lw-link (url)
  (when url
    (let* ((url (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com"))
	   (url (ppcre:regex-replace "([^/:]*://[^/]*)forum-bots\.effectivealtruism.org" url "\\1forum.effectivealtruism.org")))
      url)))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun vote-buttons (base-score &key (with-buttons t) vote-count post-id af-score as-text extended-score extended-vote-style)
  (labels ((button (vote-type)
	     (when with-buttons
	       <button type="button" class=("vote ~A" vote-type) data-vote-type=vote-type data-target-type=(if post-id "Post" "Comment") tabindex="-1" disabled autocomplete="off"></button>))
	   (text ()
	     (if (and af-score (/= af-score 0))
		 (format nil "LW: ~A AF: ~A" base-score af-score)
		 (pretty-number base-score "point")))
	   (extended-text ()
	     (alist-bind
	      (agreement agree disagree) extended-score
	      ;; LW uses agreement, EAF uses agree and disagree
	       (case extended-vote-style
		 (:ea
		  (format nil #.(uiop:strcat "~D" #\HAIR_SPACE #\RATIO #\HAIR_SPACE "~D")
			  (or agree 0)
			  (or disagree 0)))
		 (:lw
		  (pretty-number (or agree agreement 0))))))
	   (extended-tooltip ()
	     (alist-bind
	      (agreement-vote-count agree disagree) extended-score
	      ;; LW uses agreement-vote-count
	      (case extended-vote-style
		(:ea
		 (format nil "Total points: ~D" (+ (or agree 0) (or disagree 0))))
		(:lw
		 (votes-to-tooltip (or agreement-vote-count 0))))))
	   (voting (class tooltip text)
	     <div class=(safe ("~A voting-controls" class))
	          (with-html-stream-output (:stream stream)
		    (when post-id (format stream "data-post-id='~A' " post-id))
		    (unless (string-equal class "karma")
		      (format stream "data-vote-axis='~A' " class)))>
	       (button "upvote")
	       <span class="karma-value" title=tooltip>(safe text)</span>
	       (button "downvote")
	     </div>))
    (if as-text
	(hash-cond (make-hash-table)
		   (base-score :karma (list (text) (votes-to-tooltip vote-count)))
		   (extended-score :agreement (list (extended-text)
						    (extended-tooltip))))
	(progn
	  (when base-score
	    (voting "karma" (votes-to-tooltip vote-count) (text)))
	  (when extended-vote-style
	    (voting "agreement"
		    (extended-tooltip)
		    (extended-text)))))))
