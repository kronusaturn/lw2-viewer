(in-package #:lw2.backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'get-page-body))

(defun string-to-existing-keyword (string)
  (or (find-symbol (json:camel-case-to-lisp string) (find-package '#:keyword))
      string))

(define-backend-operation decode-graphql-json backend-arbital (json-string)
  (json:decode-json-from-string json-string))

(define-backend-function get-page-body (params page-type)
  (backend-arbital
    (let* ((query (json:encode-json-to-string params))
	   (fn (lambda () (block nil
			    (loop
			       (handler-case
				   (return
				     (sb-sys:with-deadline (:seconds 600)
				       (sb-ext:octets-to-string
					(drakma:http-request (case page-type
							       (:explore "https://arbital.com/json/explore/")
							       (t "https://arbital.com/json/primaryPage/"))
							     :method :post
							     :content query)
					:external-format :utf-8)))
				 (t () nil))))))
	   (json:*json-identifier-name-to-lisp* #'identity)
	   (json:*identifier-name-to-key* #'string-to-existing-keyword))
      (lw2-graphql-query-timeout-cached fn "post-body-json" (format nil "~A~@[~A~]" query page-type) :revalidate nil))))

(in-package #:lw2-viewer)

(named-readtables:in-readtable html-reader)

(defvar *arbital-context*)

(defun arbital-markdown-to-html (markdown stream)
  (let*
      ((markdown (regex-replace-all "\\[summary(?:\\(.*?\\))?:(?:[^][]|\\[.*\\])*\\]" markdown ""))
       (markdown (regex-replace-body (#'url-scanner markdown)
				     (regex-replace-all "[_*]" (match) "\\\\\\&")))
       (markdown (regex-replace-body ("\\[[-+]?([^] ]*)(?: ([^]]*?))?\\](?!\\()" markdown)
	 (let ((tag (reg 0))
	       (text (reg 1)))
	   (cond
	     ((ppcre:scan "^http" tag)
	      (format nil "<a href=\"~A\">~A</a>" (encode-entities tag) text))
	     ((ppcre:scan ":$" tag)
	      (match))
	     (t
	      (let ((page-data (cdr (assoc tag *arbital-context* :test #'string=))))
		(if-let (page-alias (cdr (assoc :alias page-data)))
			(format nil "<a href=\"/p/~A~@[?l=~A~]\">~A</a>" (encode-entities page-alias) (encode-entities tag) (or text (cdr (assoc :title page-data))))
			(format nil "<span class=\"redlink\" title=\"~A\">~A</span>" tag (or text tag)))))))))
       (markdown (regex-replace-body ("(?<!\\\\)\\$(.*?)(?<!\\\\)\\$" markdown)
				     (format nil "\\(~A\\)"
					     (regex-replace-all "[_*]" (reg 0) "\\\\\\&")))))
    (write-sequence (clean-html* (markdown:parse markdown)) stream)))

(defun arbital-meta-block (page-data all-data type)
  (let* ((creator-id (cdr (assoc :page-creator-id page-data)))
	 (user (cdr (assoc creator-id (cdr (assoc :users all-data)) :test #'string=))))
    <a class="author" href=("/p/~A" creator-id) data-userid=creator-id>
      (format nil "~{~A~^ ~}" (map 'list (lambda (x) (cdr (assoc x user))) '(:first-name :last-name)))
    </a>)
  (multiple-value-bind (pretty-time js-time) (pretty-time (cdr (assoc :page-created-at page-data)) :loose-parsing t)
    (cond
      ((eq type :comment)
       <a class="date" href=("#comment-~A" (cdr (assoc :page-id page-data))) data-js-date=js-time>
         (progn pretty-time)
       </a>)
      (t
       <span class="date" data-js-date=js-time>
         (progn pretty-time)
       </span>))))

(define-component view-arbital-page (id page-alias page-type)
  (:http-args '((l :type (or string null))))
  (let* ((id (or id l))
	 (all-data (lw2.backend:get-page-body (list-cond
					       (page-alias (cons :page-alias page-alias))
					       (id (cons :lens-id id)))
					      page-type))
	 (page-data (cdr (assoc
			  (or id
			      (cdr (assoc :page-id (cdr (assoc :result all-data))))
			      (cdr (assoc :primary-page-id (cdr (assoc :result all-data)))))
			  (cdr (assoc :pages all-data))
			  :test #'string=))))
    (renderer ()
      (let ((*arbital-context* (cdr (assoc :pages all-data))))
	(emit-page (*html-output* :title (cdr (assoc :title page-data)))
	    <main class="post">
	    <h1 class="post-title">(cdr (assoc :title page-data))</h1>
	    <div class="post-meta">
		(arbital-meta-block page-data all-data :page)
	    </div>
	    <div class="body-text post-body">
		(with-html-stream-output
		    (when (assoc :text page-data)
		    (arbital-markdown-to-html (cdr (assoc :text page-data))
						*html-output*)))
		(dolist (page-list-data '((:child-ids "Children")
					(:parent-ids "Parents")))
		(destructuring-bind (page-list-id page-list-name) page-list-data
		    <p>(progn page-list-name):
		    (labels
			((list-pages (page-list)
			    <ul>
			    (dolist (c page-list)
				(let ((page-data (cdr (assoc c (cdr (assoc :pages all-data)) :test #'string=))))
				<li><a href=("/p/~A~@[?l=~A~]" (cdr (assoc :alias page-data)) c)>(cdr (assoc :title page-data))</a>
				    (when-let (page-list (cdr (assoc page-list-id page-data)))
					    (list-pages page-list))
				</li>))
			    </ul>))
			(when-let (page-list (cdr (assoc page-list-id page-data)))
				(list-pages page-list)))
		    </p>))
	    </div>
	    </main>
	    <div class="comments" id="comments">
	    (labels ((arbital-comments (comment-list depth)
			<ul class="comment-thread">
			(dolist (c comment-list)
			    (let ((comment-data (cdr (assoc c (cdr (assoc :pages all-data)) :test #'string=))))
			    <li class=("comment-item ~A" (if (evenp depth) "depth-odd" "depth-even")) id=("#comment-~A" (cdr (assoc :page-id comment-data)))>
				<div class="comment">
				<div class="comment-meta">
				    (arbital-meta-block comment-data all-data :comment)
				</div>
				<div class="comment-body body-text">
				    (with-html-stream-output
					(arbital-markdown-to-html (cdr (assoc :text comment-data)) *html-output*))
				</div>
				</div>
				(when-let (comment-list (cdr (assoc :comment-ids comment-data)))
					(arbital-comments comment-list (1+ depth)))
			    </li>))
			</ul>))
		(when-let (comment-list (cdr (assoc :comment-ids page-data)))
			(arbital-comments comment-list 0)))
	    </div>)))))

(define-route 'arbital-site 'standard-route :name 'view-arbital-root :uri "/" :handler (route-component view-arbital-page () nil "84c" :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-page :regex "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-explore :regex "/explore/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :explore))
;(define-route 'arbital-site 'regex-route :name 'view-root :uri "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) "1rf" "probability"))
;(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page "7hh" "expected_utility_formalism"))
