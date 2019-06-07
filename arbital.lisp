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
				     (sb-ext:octets-to-string
				      (drakma:http-request (case page-type
							     (:explore "https://arbital.com/json/explore/")
							     (t "https://arbital.com/json/primaryPage/"))
							   :method :post
							   :content query)
				      :external-format :utf-8))
				 (t () nil))))))
	   (json:*json-identifier-name-to-lisp* #'identity)
	   (json:*identifier-name-to-key* #'string-to-existing-keyword))
      (lw2-graphql-query-timeout-cached fn "post-body-json" (format nil "~A~@[~A~]" query page-type) :revalidate t))))

(in-package #:lw2-viewer)

(named-readtables:in-readtable html-reader)

(defvar *arbital-context*)

(defun arbital-markdown-to-html (markdown stream)
  (let*
      ((markdown (regex-replace-all "\\[summary:(?:[^][]|\\[.*\\])*\\]" markdown ""))
       (markdown (regex-replace-body (#'url-scanner markdown)
				     (regex-replace-all "[_*]" (match) "\\\\\\&")))
       (markdown (regex-replace-body ("\\[-?([^] ]*)(?: (.*?))?\\](?!\\()" markdown)
	 (let ((tag (reg 0))
	       (text (reg 1)))
	   (cond
	     ((ppcre:scan "^http" tag)
	      (format nil "<a href=\"~A\">~A</a>" tag text))
	     ((ppcre:scan ":$" tag)
	      (match))
	     (t
	      (let ((page-data (cdr (assoc tag *arbital-context* :test #'string=))))
		(if-let (page-alias (cdr (assoc :alias page-data)))
			(format nil "<a href=\"/p/~A~@[?l=~A~]\">~A</a>" page-alias tag (or text (cdr (assoc :title page-data))))
			(format nil "<span class=\"redlink\" title=\"~A\">~A</span>" tag (or text tag)))))))))
       (markdown (regex-replace-all "(?<!\\\\)\\$(.*?)(?<!\\\\)\\$" markdown "\\\\(\\1\\\\)")))
    (write-sequence (clean-html* (markdown:parse markdown)) stream)))

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
      (emit-page (*html-output*)
	<main class="post">
	  <h1 class="post-title">(cdr (assoc :title page-data))</h1>
          <div class="body-text post-body">
	    (with-html-stream-output
	        (when (assoc :text page-data)
	          (let ((*arbital-context* (cdr (assoc :pages all-data))))
		    (arbital-markdown-to-html (cdr (assoc :text page-data))
					      *html-output*))))
	    (dolist (page-list-data '((:child-ids "Children")
				      (:parent-ids "Parents")
				      (:comment-ids "Comments")))
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
        </main>))))

(define-route 'arbital-site 'standard-route :name 'view-arbital-root :uri "/" :handler (route-component view-arbital-page () nil "84c" :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-page :regex "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-explore :regex "/explore/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :explore))
;(define-route 'arbital-site 'regex-route :name 'view-root :uri "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) "1rf" "probability"))
;(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page "7hh" "expected_utility_formalism"))
