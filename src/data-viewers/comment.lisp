(uiop:define-package #:lw2.data-viewers.comment
  (:use #:cl #:lw2.html-reader #:lw2.utils #:lw2.schema-type #:lw2.context #:lw2.user-context #:lw2.backend #:lw2.links #:lw2.interface-utils #:lw2.sites #:lw2.clean-html #:lw2.lmdb #:lw2.backlinks)
  (:export #:*comment-individual-link* #:comment-to-html))

(in-package #:lw2.data-viewers.comment)

(named-readtables:in-readtable html-reader)

(defparameter *comment-individual-link* nil)

(define-schema-type :comment ()
  ((comment-id string :alias :--id)
   (user-id string)
   (posted-at string)
   (highlight-new boolean :graphql-ignore t)
   (replied list :graphql-ignore t)
   (post-id (or null simple-string))
   (tag-id (or null simple-string))
   (base-score (or null fixnum))
   (af-base-score (or null fixnum))
   (page-url (or null string) :context-not :user-index) ; page-url sometimes causes "Cannot read property '_id' of undefined" error
   (parent-comment list :context :index :subfields (:--id :user-id :post-id))
   (parent-comment-id (or null string))
   (child-count (or null fixnum) :graphql-ignore t)
   (children list :graphql-ignore t)
   (af boolean :backend-type backend-alignment-forum)
   (vote-count (or null fixnum))
   (retracted boolean)
   (deleted-public boolean)
   (answer boolean :backend-type backend-q-and-a)
   (parent-answer-id (or null string) :backend-type backend-q-and-a)
   (nominated-for-review t :backend-type backend-lw2)
   (reviewing-for-review t :backend-type backend-lw2)
   (top-level-comment list :backend-type backend-lw2 :subfields (:nominated-for-review :reviewing-for-review))
   (latest-children list
		    :backend-type backend-shortform
		    :context :shortform
		    :subfields (:--id :user-id :posted-at :post-id :base-score :af-base-score :page-url
				:parent-comment-id :af :vote-count :retracted :deleted-public :html-body))
   (html-body string)))

(defun comment-to-html (out-stream comment &key with-post-title)
  (if (or (cdr (assoc :deleted comment)) (cdr (assoc :deleted-public comment)))
      (format out-stream "<div class=\"comment deleted-comment\"><div class=\"comment-meta\"><span class=\"deleted-meta\">[ ]</span></div><div class=\"body-text comment-body\">[deleted]</div></div>")
      (schema-bind (:comment comment :auto :context :index)
	(unless post-id (return-from comment-to-html)) ; XXX fixme
        (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
	  <div class=("comment~{ ~A~}"
		      (list-cond
		       ((and (logged-in-userid user-id)
			     (< (* 1000 (local-time:timestamp-to-unix (local-time:now))) (+ js-time 15000)))
			"just-posted-comment")
		       (highlight-new "comment-item-highlight")
		       (retracted "retracted")))
	       data-post-id=post-id
	       data-tag-id=tag-id>
	    <div class="comment-meta">
	      (if (user-deleted user-id)
		  <span class="author">[deleted]</span>
	          <a class=("author~:[~; own-user-author~]" (logged-in-userid user-id))
		     href=("/users/~A" (encode-entities (get-user-slug user-id)))
		     data-userid=user-id
		     data-full-name=(get-user-full-name user-id)>
		    (get-username user-id)
	          </a>)
	      <a class="date" href=(generate-post-link post-id comment-id) data-js-date=js-time> (safe pretty-time) (safe (pretty-time-js))</a>
	      (when replied <a class="replied" title="You have replied to this comment" href=(apply 'generate-post-link replied)></a>)
	      (vote-buttons base-score :with-buttons (not with-post-title) :vote-count vote-count :af-score (and af af-base-score))
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
				   (get-username user-id)</a>’s
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
							      (or (markdown-source :comment comment-id html-body)
								  html-body)))))>
		  (with-html-stream-output
		    (let ((*before-clean-hook* (lambda () (clear-backlinks post-id comment-id)))
			  (*link-hook* (lambda (link)
					 (add-backlink link post-id comment-id)))
			  (lw2.lmdb:*memoized-output-stream* out-stream))
		      (clean-html* html-body)))
	      </div>
	      (backlinks-to-html (get-backlinks post-id comment-id) (format nil "~A-~A" post-id comment-id))
	      (when (and (not with-post-title) (logged-in-userid))
	        <script>initializeCommentControls\(\)</script>)
	    </div>))))
