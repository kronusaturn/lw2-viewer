(uiop:define-package #:lw2.data-viewers.post
  (:use #:cl #:lw2.schema-types #:lw2.utils #:lw2.sites #:lw2.backend #:lw2.context #:lw2.clean-html #:lw2.schema-type #:lw2.html-reader #:lw2.interface-utils #:lw2.user-context #:lw2.links #:lw2.backlinks)
  (:export #:post-headline-to-html #:post-body-to-html))

(in-package #:lw2.data-viewers.post)

(named-readtables:in-readtable html-reader)

(defgeneric rectify-post* (backend post) ; TODO this should go in a more generic postprocessing method
  (:method ((backend t) post) post)
  (:method ((backend backend-feed-crossposts) post)
    (if (cdr (assoc :url post))
	post
	(acons :url (cdr (assoc :feed-link post)) post))))

(defun rectify-post (post) (rectify-post* *current-backend* post))

(defun post-section-to-html (post &key skip-section)
  (schema-bind (:post (rectify-post post) (user-id frontpage-date curated-date meta af draft))
    (multiple-value-bind (class title href)
	(cond (af (if (eq skip-section :alignment-forum) nil (values "alignment-forum" "View Alignment Forum posts" "/index?view=alignment-forum")))
	      ; show alignment forum even if skip-section is t
	      ((eq skip-section t) nil)
	      (draft nil)
	      (curated-date (if (eq skip-section :featured) nil (values "featured" "View Featured posts" "/index?view=featured")))
	      (frontpage-date (if (eq skip-section :frontpage) nil (values "frontpage" "View Frontpage posts" "/")))
	      (meta (if (eq skip-section :meta) nil (values "meta" "View Meta posts" "/index?view=meta")))
	      (t (if (eq skip-section :personal) nil (values "personal" (format nil "View posts by ~A" (get-username user-id)) (format nil "/users/~A?show=posts" (get-user-slug user-id))))))
      <a class=("post-section ~A" class) title=title href=href></a>)))

(defun post-meta-to-html (post context skip-section)
  (schema-bind (:post (rectify-post post) :auto)
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      <div class="post-meta">
        (if (user-deleted user-id)
	    <span class="author">[deleted]</span>
            <a class=("author~{ ~A~}" (list-cond
		         ((logged-in-userid user-id) "own-user-author")))
               href=("/users/~A" (get-user-slug user-id))
               data-userid=user-id
	       data-full-name=(get-user-full-name user-id)>
              (get-username user-id)
	    </a>)
        <div class="date" data-js-date=js-time>(progn pretty-time)</div>
	(vote-buttons base-score :with-buttons (eq context :body) :vote-count vote-count :post-id post-id :af-score (and (eq context :body) af af-base-score))
        <a class="comment-count" href=("~A#comments" (if (eq context :body) "" (generate-post-link post)))>
	  (safe (pretty-number (or comment-count 0) "comment"))
	</a>
        (when (and (eq context :listing) word-count)
	  <span class="read-time" title=(safe (pretty-number word-count "word"))>(max 1 (round word-count 300))<span> min read</span></span>)
	(if page-url <a class="lw2-link" href=(clean-lw-link page-url)>(main-site-abbreviation *current-site*)<span> link</span></a>)
	(when (nonzero-number-p nomination-count-2018)
	  <a href=("~A#nominations" (if (eq context :body) "" (generate-post-link post))) class="nomination-count">(safe (pretty-number nomination-count-2018 "nomination"))</a>)
	(when (nonzero-number-p review-count-2018)
	  <a href=("~A#reviews" (if (eq context :body) "" (generate-post-link post))) class="review-count">(safe (pretty-number review-count-2018 "review"))</a>)
        (with-html-stream-output (post-section-to-html post :skip-section skip-section))
        (when (and (eq context :body) tags)
          <div id="tags">
            (dolist (tag tags) (alist-bind ((text string)) tag <a href=("/tags/~A" text)>(progn text)</a>))
	  </div>)
        (when (and (eq context :listing) url)
	  <div class="link-post-domain">("(~A)" (puri:uri-host (puri:parse-uri (string-trim " " url))))</div>)
      </div>)))

(defun post-headline-to-html (post &key skip-section need-auth)
  (schema-bind (:post (rectify-post post) (post-id user-id url question title))
    <h1 class=("listing~{ ~A~}" (list-cond
				 (url "link-post-listing")
				 (question "question-post-listing")
				 ((logged-in-userid user-id) "own-post-listing")))>
      (if url <a href=(convert-any-link (string-trim " " url))>&#xf0c1;</a>)
      <a href=(generate-post-auth-link post nil nil need-auth)>
	(if question <span class="post-type-prefix">[Question] </span>)
	(safe (clean-text-to-html title))
      </a>
      (if (logged-in-userid user-id) <a class="edit-post-link button" href=("/edit-post?post-id=~A" post-id)></a>)
    </h1>
    (post-meta-to-html post :listing skip-section)))

(defun post-body-to-html (post)
  (schema-bind (:post (rectify-post post) (post-id url question title html-body) :context :body)
    <main class=("post~{ ~A~}" (list-cond
				(url "link-post")
				(question "question-post")))>
      <h1 class="post-title">
        (if question <span class="post-type-prefix">[Question] </span>)
        (safe (clean-text-to-html title :hyphenation nil))
      </h1>
      (with-html-stream-output (post-meta-to-html post :body nil))
      <div class="body-text post-body">
        (if url <p><a class="link-post-link" href=(convert-any-link (string-trim " " url))>Link post</a></p>)
	(with-html-stream-output
	  (let ((*before-clean-hook* (lambda () (clear-backlinks post-id)))
		(*link-hook* (lambda (link) (add-backlink link post-id)))
		  (lw2.lmdb:*memoized-output-stream* *html-output*))
	      (clean-html* (or html-body "") :with-toc t :post-id post-id) *html-output*))
      </div>
      (backlinks-to-html (get-backlinks post-id) post-id)
      (with-html-stream-output #|(post-meta-to-html post :body nil) TODO: don't use js to insert bottom-post-meta|#)
    </main>))
