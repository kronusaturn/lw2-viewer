(uiop:define-package #:lw2.data-viewers.post
  (:use #:cl #:lw2.schema-types #:lw2.utils #:lw2.sites #:lw2.backend #:lw2.context #:lw2.clean-html #:lw2.schema-type #:lw2.html-reader #:lw2.interface-utils #:lw2.user-context #:lw2.links)
  (:export #:post-headline-to-html #:post-body-to-html))

(in-package #:lw2.data-viewers.post)

(named-readtables:in-readtable html-reader)

(defun post-section-to-html (post &key skip-section)
  (schema-bind (:post post (user-id frontpage-date curated-date meta af draft))
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
  (schema-bind (:post post :auto)
    (multiple-value-bind (pretty-time js-time) (pretty-time posted-at)
      <div class="post-meta">
        <a class=("author~{ ~A~}" (list-cond
		     ((logged-in-userid user-id) "own-user-author")))
           href=("/users/~A" (get-user-slug user-id))
           data-userid=user-id
	   data-full-name=(get-user-full-name user-id)>
          (get-username user-id)
	</a>
        <div class="date" data-js-date=js-time>(progn pretty-time)</div>
        <div class="karma" data-post-id=post-id>
          <span class="karma-value" title=(votes-to-tooltip vote-count)>(safe (pretty-number base-score "point"))</span>
        </div>
        <a class="comment-count" href=("~A#comments" (if (eq context :body) "" (generate-post-link post)))>
	  (safe (pretty-number (or comment-count 0) "comment"))
	</a>
        (when (and (eq context :listing) word-count)
	  <span class="read-time" title=(safe (pretty-number word-count "word"))>(max 1 (round word-count 300))<span> min read</span></span>)
	(if page-url <a class="lw2-link" href=(clean-lw-link page-url)>(main-site-abbreviation *current-site*)<span> link</span></a>)
        (with-html-stream-output (post-section-to-html post :skip-section skip-section))
        (when (and (eq context :body) tags)
          <div id="tags">
            (dolist (tag tags) (alist-bind ((text string)) tag <a href=("/tags/~A" text)>(progn text)</a>))
	  </div>)
        (when (and (eq context :listing) url)
	  <div class="link-post-domain">("(~A)" (puri:uri-host (puri:parse-uri (string-trim " " url))))</div>)
      </div>)))

(defun post-headline-to-html (post &key skip-section need-auth)
  (schema-bind (:post post (post-id user-id url question title))
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
  (schema-bind (:post post (post-id url question title html-body) :qualifier :body)
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
	    (write-sequence (clean-html* (or html-body "") :with-toc t :post-id post-id) *html-output*))
      </div>
      (with-html-stream-output #|(post-meta-to-html post :body nil) TODO: don't use js to insert bottom-post-meta|#)
    </main>))
