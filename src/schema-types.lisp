(uiop:define-package #:lw2.schema-types
    (:use #:cl #:lw2.schema-type #:lw2.backend-modules))

(in-package #:lw2.schema-types)

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
   (nomination-count-2018 (or null fixnum) :backend-type backend-lw2)
   (review-count-2018 (or null fixnum) :backend-type backend-lw2)
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

(define-schema-type :comment ()
  ((comment-id string :alias :--id)
   (user-id string)
   (posted-at string)
   (highlight-new boolean :graphql-ignore t)
   (replied list :graphql-ignore t)
   (post-id string)
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

(define-schema-type :tag ()
  ((tag-id string :alias :--id)
   (name string)
   (slug string)
   (post-count (or null fixnum))
   (description list
		:context :body
		:subfields (:html))))
