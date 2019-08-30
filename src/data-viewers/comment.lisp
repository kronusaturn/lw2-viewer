(uiop:define-package #:lw2.data-viewers.comment
  (:use #:cl #:lw2.html-reader #:lw2.utils #:lw2.schema-type #:lw2.schema-types #:lw2.context #:lw2.user-context #:lw2.backend #:lw2.links #:lw2.interface-utils #:lw2.sites #:lw2.clean-html #:lw2.lmdb)
  (:export #:*comment-individual-link* #:comment-to-html))

(in-package #:lw2.data-viewers.comment)

(named-readtables:in-readtable html-reader)

(defparameter *comment-individual-link* nil)

(defun comment-to-html (out-stream comment &key with-post-title)
  (if (or (cdr (assoc :deleted comment)) (cdr (assoc :deleted-public comment)))
      (format out-stream "<div class=\"comment deleted-comment\"><div class=\"comment-meta\"><span class=\"deleted-meta\">[ ]</span></div><div class=\"body-text comment-body\">[deleted]</div></div>")
      (schema-bind (:comment comment :auto :context :index)
        (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
	  <div class=("comment~{ ~A~}"
		      (list-cond
		       ((and (logged-in-userid user-id)
			     (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000)))
			"just-posted-comment")
		       (highlight-new "comment-item-highlight")
		       (retracted "retracted")))>
	    <div class="comment-meta">
	      (if (user-deleted user-id)
		  <span class="author">[deleted]</span>
	          <a class=("author~:[~; own-user-author~]" (logged-in-userid user-id))
		     href=("/users/~A" (encode-entities (get-user-slug user-id)))
		     data-userid=user-id
		     data-full-name=(get-user-full-name user-id)>
		    (get-username user-id)
	          </a>)
	      <a class="date" href=(generate-post-link post-id comment-id) data-js-date=js-time> (safe pretty-time) </a>
	      (vote-buttons base-score :vote-count vote-count)
	      (when af <span class="alignment-forum">AF</span>)     
	      <a class="permalink" href=("~A/~A/~A"
					 (generate-post-link post-id)
					 (cond ((or answer parent-answer-id) "answer") (t "comment"))
					 comment-id)
		 title="Permalink"></a>
	      (with-html-stream-output
		(when page-url
		  <a class="lw2-link" href=(clean-lw-link page-url) title=(main-site-abbreviation *current-site*)></a>)
		(if with-post-title
		    <div class="comment-post-title">
		      (with-html-stream-output
			  (when parent-comment
			    (alist-bind ((user-id string)
					 (post-id string)
					 (parent-id string :--id))
					parent-comment
			      <span class="comment-in-reply-to">in reply to:
				<a href=("/users/~A" (get-user-slug user-id))
				   class=("inline-author~:[~; own-user-author~]" (logged-in-userid user-id))
				   data-userid=(progn user-id)>
				   (get-username user-id)</a>'s
				<a href=(generate-post-link post-id parent-id)>comment</a>
				(progn " ")
			      </span>)))
			<span class="comment-post-title2">on: <a href=(generate-post-link post-id)>(safe (clean-text-to-html (get-post-title post-id)))</a></span>
		      </div>
		  (when parent-comment-id
		    (if *comment-individual-link*
			<a class="comment-parent-link" href=(progn parent-comment-id) title="Parent"></a>
			<a class="comment-parent-link" href=("#comment-~A" parent-comment-id)>Parent</a>)))
		(when children
		  <div class="comment-child-links">
		    Replies:
		    (with-html-stream-output
		      (dolist (child children)
			(alist-bind ((comment-id string)
				     (user-id string))
				    child
			  <a href=("#comment-~A" comment-id)>(">~A" (get-username user-id))</a>)))
		  </div>)
		<div class="comment-minimize-button"
		     data-child-count=(progn child-count)>
		</div>)
	      </div>
	      <div class="body-text comment-body" (safe ("~@[ data-markdown-source=\"~A\"~]"
					       (if (logged-in-userid user-id)
						   (encode-entities
						    (or (cache-get "comment-markdown-source" comment-id)
							html-body)))))>
		(with-html-stream-output (write-sequence (clean-html* html-body) out-stream))
              </div>
	    </div>))))
