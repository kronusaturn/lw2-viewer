(uiop:define-package #:lw2.interface-utils
  (:use #:cl #:lw2.links #:lw2.html-reader)
  (:export #:pretty-time #:pretty-time-js #:pretty-time-html
	   #:pretty-number #:generate-post-auth-link #:clean-lw-link #:votes-to-tooltip #:vote-buttons))

(in-package #:lw2.interface-utils)

(named-readtables:in-readtable html-reader)

(defun pretty-time (timestring &key format loose-parsing)
  (let ((time (if loose-parsing
		  (chronicity:parse timestring)
		  (local-time:parse-timestring timestring))))
    (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	    (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-time-js ()
  "<script async src='data:text/javascript,prettyDate()'></script>")

(defun pretty-time-html (timestring)
  (multiple-value-bind (pretty-time js-time) (pretty-time timestring)
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
    (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com")))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun vote-buttons (base-score &key (with-buttons t) vote-count post-id af-score as-text extended-score all-votes)
  (labels ((button (vote-type)
	     (when with-buttons
	       <button type="button" class=("vote ~A" vote-type) data-vote-type=vote-type data-target-type=(if post-id "Posts" "Comments") tabindex="-1" disabled></button>))
	   (text ()
	     (if (and af-score (/= af-score 0))
		 (format nil "LW: ~A AF: ~A" base-score af-score)
		 (pretty-number base-score "point")))
	   (score-counts ()
	     (let ((hash (make-hash-table :test 'equal)))
	       (loop for vote in all-votes
		  for agreement = (cdr (assoc :agreement (cdr (assoc :extended-vote-type vote))))
		  do (when agreement
		       (incf (gethash agreement hash 0))))
	       hash))
	   (extended-text (agree-count disagree-count)
	     (format nil "~D : ~D" agree-count disagree-count))
	   (extended-tooltip (score-counts agree-count disagree-count)
	     (format nil "~D agree (~D strongly), ~D disagree (~D strongly); meaningless number: ~D"
		     agree-count
		     (gethash "bigUpvote" score-counts 0)
		     disagree-count
		     (gethash "bigDownvote" score-counts 0)
		     (cdr (assoc :agreement extended-score))))
	   (voting (class tooltip text)
	     <div class=class data-post-id=post-id>
	       (button "upvote")
	       <span class="karma-value" title=tooltip>(safe text)</span>
	       (button "downvote")
	     </div>))
    (when (or base-score extended-score)
      (if as-text
	  (text)
	  (progn
	    (when base-score
	      (voting "karma" (votes-to-tooltip vote-count) (text)))
	    (when extended-score
	      (let* ((score-counts (score-counts))
		     (agree-count (+ (gethash "smallUpvote" score-counts 0) (gethash "bigUpvote" score-counts 0)))
		     (disagree-count (+ (gethash "smallDownvote" score-counts 0) (gethash "bigDownvote" score-counts 0))))
		(voting "agreement"
			(extended-tooltip score-counts agree-count disagree-count)
			(extended-text agree-count disagree-count)))))))))
