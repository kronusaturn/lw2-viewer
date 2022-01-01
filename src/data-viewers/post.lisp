(uiop:define-package #:lw2.data-viewers.post
  (:use #:cl #:lw2.utils #:lw2.sites #:lw2.backend #:lw2.context #:lw2.clean-html #:lw2.schema-type #:lw2.html-reader #:lw2.interface-utils #:lw2.user-context #:lw2.links #:lw2.backlinks)
  (:import-from #:alexandria #:when-let)
  (:export #:post-headline-to-html #:post-body-to-html))

(in-package #:lw2.data-viewers.post)

(named-readtables:in-readtable html-reader)

(define-schema-type :post ()
  ((post-id string :alias :--id)
   (slug string)
   (title string)
   (user-id string)
   (coauthors (or null list) :backend-type backend-lw2 :subfields (:--id))
   (url (or null string))
   (feed-link (or null string) :backend-type backend-feed-crossposts)
   (canonical-source (or null string) :backend-type backend-feed-crossposts)
   (posted-at string)
   (base-score (or null fixnum))
   (af-base-score (or null fixnum))
   (comment-count (or null fixnum))
   (page-url (or null string))
   (word-count (or null fixnum))
   (frontpage-date (or null string))
   (curated-date (or null string))
   (legacy-id t :backend-type backend-lw2)
   (meta boolean)
   (af boolean :backend-type backend-alignment-forum)
   (draft boolean)
   (question boolean :backend-type backend-q-and-a)
   ;; todo: allow recursive schema types and clean this up
   (target-post-relations list
			  :context :body
			  :backend-type backend-related-questions
			  :subfields ((:target-post :--id :slug :title :user-id :url :feed-link
						    :posted-at :base-score :comment-count :page-url
						    :word-count :frontpage-date :curated-date :meta
						    :af :question :vote-count)))
   (source-post-relations list
			  :context :body
			  :backend-type backend-related-questions
			  :subfields ((:source-post :--id :slug :title :user-id :url :feed-link
						    :posted-at :base-score :comment-count :page-url
						    :word-count :frontpage-date :curated-date :meta
						    :af :question :vote-count)))
   (vote-count (or null fixnum))
   (nomination-count-2019 (or null fixnum) :backend-type backend-lw2)
   (review-count-2019 (or null fixnum) :backend-type backend-lw2)
   (is-event boolean :backend-type backend-events)
   (local-start-time (or null string) :backend-type backend-events)
   (local-end-time (or null string) :backend-type backend-events)
   (location (or null string) :backend-type backend-events)
   (google-location (or null list) :backend-type backend-events)
   (contact-info (or null string) :backend-type backend-events)
   (comment-sort-order (or null string) :backend-type backend-lw2)
   (tags list :graphql-ignore t)
   (submit-to-frontpage boolean :backend-type backend-lw2-misc-features)
   (html-body (or null string) :context :body)))

(defgeneric rectify-post* (backend post) ; TODO this should go in a more generic postprocessing method
  (:method ((backend t) post) post)
  (:method ((backend backend-lw2-misc-workarounds) post)
    (let* ((post (if (next-method-p) (call-next-method) post))
	   (url (cdr (assoc :url post)))
	   (url (and url (string-trim " " url))))
      (cond ((null url) post)
	    ((or (uiop:string-prefix-p "http" url) (uiop:string-prefix-p "/" url)) post)
	    (t (acons :url (concatenate 'string "http://" url) post)))))
  (:method ((backend backend-feed-crossposts) post)
    (if (cdr (assoc :url post))
	post
	(acons :url (cdr (assoc :feed-link post)) post))))

(defun rectify-post (post) (rectify-post* *current-backend* post))

(defgeneric tag-list-to-html (backend tags)
  (:method ((backend backend-accordius) tags)
    (dolist (tag tags) (alist-bind ((text string)) tag <a href=("/tags/~A" text)>(progn text)</a>)))
  (:method ((backend backend-lw2-tags) tags)
    (dolist (tag tags) (alist-bind ((name string) (slug string)) (cdr (assoc :tag tag)) <a href=("/tag/~A" slug)>(progn name)</a>))))

(defun qualified-linking (url meta-location)
  <nav class="qualified-linking">
    <input type="checkbox" tabindex="-1" id=("qualified-linking-toolbar-toggle-checkbox-~(~A~)" meta-location)>
    <label for=("qualified-linking-toolbar-toggle-checkbox-~(~A~)" meta-location)><span>&#xf141\;</span></label>
    <div class="qualified-linking-toolbar">
      <a href=url>Post permalink</a>
      <a href=("~A?comments=false" url)>Link without comments</a>
      <a href=("~A?hide-nav-bars=true" url)>Link without top nav bars</a>
      <a href=("~A?comments=false&hide-nav-bars=true" url)>Link without comments or top nav bars</a>
    </div>
  </nav>)

(defun post-section-to-html (post &key skip-section)
  (schema-bind (:post (rectify-post post) (user-id frontpage-date curated-date meta is-event af draft))
    (multiple-value-bind (class title href)
	(cond (af (if (eq skip-section :alignment-forum) nil (values "alignment-forum" "View Alignment Forum posts" "/index?view=alignment-forum")))
	      ; show alignment forum even if skip-section is t
	      ((eq skip-section t) nil)
	      (draft nil)
	      (curated-date (if (eq skip-section :featured) nil (values "featured" "View Featured posts" "/index?view=featured")))
	      (frontpage-date (if (eq skip-section :frontpage) nil (values "frontpage" "View Frontpage posts" "/")))
	      (meta (if (eq skip-section :meta) nil (values "meta" "View Meta posts" "/index?view=meta")))
	      (is-event (values "events" "View Events" "/index?view=events"))
	      (t (if (eq skip-section :personal) nil (values "personal" (format nil "View posts by ~A" (get-username user-id)) (format nil "/users/~A?show=posts" (get-user-slug user-id))))))
      <a class=("post-section ~A" class) title=title href=href></a>)))

(defun post-meta-to-html (post context skip-section meta-location)
  (schema-bind (:post (rectify-post post) :auto)
    <div class=("post-meta~@[ ~(~A~)-post-meta~]" meta-location)>
      (labels ((emit-author (user-id)
		 (if (user-deleted user-id)
		     <span class="author">[deleted]</span>
                     <a class=("author~{ ~A~}" (list-cond
						((logged-in-userid user-id) "own-user-author")))
	                href=("/users/~A" (get-user-slug user-id))
			data-userid=user-id
			data-full-name=(get-user-full-name user-id)>
	               (get-username user-id)
		     </a>)))
	(if coauthors
	    <div class="coauthors">
	      (emit-author user-id)
	      (do ((remaining-coauthors coauthors (rest remaining-coauthors))) ((not remaining-coauthors))
		(with-html-stream-output (:stream stream)
		  (write-string
		   (cond ((second remaining-coauthors) ", ")
			 (t " and "))
		   stream))
		(emit-author (cdr (assoc :--id (first remaining-coauthors)))))
	    </div>
	    (emit-author user-id)))
      (pretty-time-html posted-at)
      (vote-buttons base-score :with-buttons (eq context :body) :vote-count vote-count :post-id post-id :af-score (and (eq context :body) af af-base-score))
      <a class="comment-count" href=("~A#comments" (if (eq context :body) "" (generate-item-link :post post)))>
	(safe (pretty-number (or comment-count 0) "comment"))
      </a>
      (when (and (eq context :listing) word-count)
	<span class="read-time" title=(safe (pretty-number word-count "word" :text))>(max 1 (round word-count 300))<span> min read</span></span>)
      (if page-url <a class="lw2-link" href=(clean-lw-link page-url)>(main-site-abbreviation *current-site*)<span> link</span></a>)
      (when (and legacy-id (eq context :body))
	<a class="archive-link" href=("https://web.archive.org/web/*/~A" (lw2.legacy-archive:lw-legacy-url legacy-id title))>Archive</a>)
      (when (nonzero-number-p nomination-count-2019)
	<a href=("~A#nominations" (if (eq context :body) "" (generate-item-link :post post))) class="nomination-count">(safe (pretty-number nomination-count-2019 "nomination"))</a>)
      (when (nonzero-number-p review-count-2019)
	<a href=("~A#reviews" (if (eq context :body) "" (generate-item-link :post post))) class="review-count">(safe (pretty-number review-count-2019 "review"))</a>)
      (with-html-stream-output (post-section-to-html post :skip-section skip-section))
      (when (and (eq context :body) tags)
	<div id="tags">
	  (tag-list-to-html *current-backend* tags)
	</div>)
      (when-let ((url-host (and (eq context :listing) url (ignore-errors (quri:uri-host (quri:uri (string-trim " " url)))))))
	<div class="link-post-domain">("(~A)" url-host)</div>)
      (when (eq context :body)
	(qualified-linking (generate-item-link :post post) meta-location))
    </div>))

(defun post-headline-to-html (post &key skip-section need-auth)
  (schema-bind (:post (rectify-post post) (post-id user-id url question title is-event))
    <h1 class=("listing~{ ~A~}" (list-cond
				 (url "link-post-listing")
				 (question "question-post-listing")
				 ((logged-in-userid user-id) "own-post-listing")))>
      (if url <a class="link-post-link" href=(presentable-link url)>&#xf0c1;</a>)
      <a class="post-title-link" href=(generate-post-auth-link post :need-auth need-auth :item-subtype (if is-event "event" "post"))>
	(if question <span class="post-type-prefix">[Question] </span>)
	(safe (clean-text-to-html (or (nonempty-string title) "[untitled post]")))
      </a>
      (if (logged-in-userid user-id) <a class="edit-post-link button" href=("/edit-post?post-id=~A" post-id)></a>)
    </h1>
    (post-meta-to-html post :listing skip-section nil)))

(defun post-body-to-html (post)
  (schema-bind (:post (rectify-post post) (post-id url question title html-body is-event local-start-time local-end-time location google-location contact-info) :context :body)
    <main class=("post~{ ~A~}" (list-cond
				(url "link-post")
				(question "question-post")))>
      <h1 class="post-title">
        (if question <span class="post-type-prefix">[Question] </span>)
        (safe (clean-text-to-html title :hyphenation nil))
      </h1>
      (with-html-stream-output (post-meta-to-html post :body nil :top))
      (when is-event
	(labels ((brief-date (timestamp)
		   (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+ :format '(:day #\Space :long-month #\Space :year)))
		 (brief-time (timestamp)
		   (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+ :format '(:hour12 #\: (:min 2) #\Space :ampm))))
	  <div class="event-info">
	    (alist-bind ((geometry list)
			 (google-maps-url (or null string) :url))
			google-location
	      (alist-bind ((lat (or null real)) (lng (or null real))) (cdr (assoc :location geometry))
		(when (and lat lng)
		  (let* ((north (+ lat 0.125)) (south (- lat 0.125)) (east (+ lng 0.25)) (west (- lng 0.25)))
		    <div class="map">
		      <iframe src=("https://www.openstreetmap.org/export/embed.html?bbox=~F,~F,~F,~F&layer=mapnik&marker=~F,~F" west south east north lat lng)></iframe>
		    </div>))
		(let* ((start-timestamp (and local-start-time (local-time:parse-timestring local-start-time)))
		       (end-timestamp (and local-end-time (local-time:parse-timestring local-end-time)))
		       (same-day (and start-timestamp end-timestamp (= (local-time:day-of start-timestamp) (local-time:day-of end-timestamp)))))
		  <ul>
		    (when (and lat lng)
		      <li><a href=("https://www.google.com/maps/place/~F,~F" lat lng)>[Open in Google Maps]</a> <a href=("geo:~F,~F" lat lng)>[Open in local app]</a></li>)
		    (when start-timestamp
		      <li>(brief-date start-timestamp), (brief-time start-timestamp)
		        (when end-timestamp
			  <span>—(unless same-day (format nil "~A, " (brief-date end-timestamp)))(brief-time end-timestamp)</span>)
		      </li>)
		    (when location
		      <li>(safe (clean-text-to-html location))</li>)
		    (when contact-info
		      <li>Contact: (safe (clean-text-to-html contact-info))</li>)
		    </ul>)))
	</div>))
      (when (or url (nonempty-string html-body))
        <div class="body-text post-body">
          (if url <p><a class="link-post-link" href=(presentable-link url)>Link post</a></p>)
  	  (with-html-stream-output (:stream stream)
	    (let ((*before-clean-hook* (lambda () (clear-backlinks post-id)))
		  (*link-hook* (lambda (link) (add-backlink link post-id)))
		  (lw2.lmdb:*memoized-output-stream* stream))
	      (clean-html* (or html-body "") :with-toc t :post-id post-id)))
        </div>)
      (backlinks-to-html (get-backlinks post-id) post-id)
      (when (nonempty-string html-body)
	(with-html-stream-output (post-meta-to-html post :body nil :bottom)))
    </main>))
