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
				 (t () nil))
			       (sleep 2)))))
	   (json:*json-identifier-name-to-lisp* #'identity)
	   (json:*identifier-name-to-key* #'string-to-existing-keyword))
      (lw2-graphql-query-timeout-cached fn "post-body-json" (format nil "~A~@[~A~]" query page-type) :revalidate nil))))

(in-package #:lw2-viewer)

(named-readtables:in-readtable html-reader)

(defvar *arbital-context*)

(defmethod site-stylesheets append ((site arbital-site))
  (list (generate-versioned-link "/arbital.css")))

(defmethod site-nav-bars ((site arbital-site))
  '((:secondary-bar (("about" "/about" "About" :accesskey "t")))
    (:primary-bar (("home" "/" "Home" :accesskey "h")
		   ("math" "/explore/math/" "Math")
		   ("ai-alignment" "/explore/ai_alignment/" "AI Alignment")
		   ("arbital" "/explore/Arbital/" "Arbital")))))

(defparameter *markdown-replace-string* "ouNi5iej")

(defun arbital-markdown-to-html (markdown stream)
  (let ((replacements (make-array 0 :adjustable t :fill-pointer t)))
    (labels ((markdown-protect (x)
	       (prog1 (format nil "~A-~A-" *markdown-replace-string* (fill-pointer replacements))
		 (vector-push-extend x replacements)))
	     (markdown-protect-wrap (a b c)
	       (concatenate 'string (markdown-protect a) b (markdown-protect c))))
      (let*
	  ((markdown (regex-replace-all (ppcre:create-scanner "(?<=\\S )\\*(?= )" :single-line-mode t) markdown "\\\\*"))
	   (markdown (regex-replace-all (ppcre:create-scanner "\\[.?summary(?:\\(.*?\\))?:.*?\\]$" :single-line-mode t :multi-line-mode t) markdown ""))
	   (markdown (regex-replace-body (#'url-scanner markdown)
		       (markdown-protect (match))))
	   (markdown (regex-replace-body ("\\[[-+]?([^] ]*)(?: ([^]]*?))?\\](?!\\()" markdown)
		       (let ((tag (reg 0))
			     (text (reg 1)))
			 (cond
			   ((ppcre:scan "^http" tag)
			    (markdown-protect-wrap
			     (format nil "<a href=\"~A\">" (encode-entities tag))
			     (or text tag)
			     "</a>"))
			   ((ppcre:scan ":$" tag)
			    (or text ""))
			   (t
			    (let ((page-data (cdr (assoc tag *arbital-context* :test #'string=))))
			      (if-let (page-alias (cdr (assoc :alias page-data)))
				      (markdown-protect-wrap
				       (format nil "<a href=\"/p/~A~@[?l=~A~]\">" (encode-entities page-alias) (encode-entities tag))
				       (or text (cdr (assoc :title page-data)))
				       "</a>")
				      (markdown-protect-wrap
				       (format nil "<span class=\"redlink\" title=\"~A\">" (encode-entities tag))
				       (or text tag)
				       "</span>"))))))))
	   (markdown (regex-replace-body ((ppcre:create-scanner "(?<!\\\\)(\\$\\$?)(.+?)(?<!\\\\)\\1" :single-line-mode t :multi-line-mode t) markdown)
		       (markdown-protect
			(let ((block (= (length (reg 0)) 2)))
			  (format nil "<~A class=\"arbital-math\">~A~A~A</~A>"
				  (if block "div" "span")
				  (if block "$$" "\\(")
				  (reg 1)
				  (if block "$$" "\\)")
				  (if block "div" "span"))))))
	   (html (regex-replace-body ((load-time-value (format nil "~A-(\\d+)-" *markdown-replace-string*)) (markdown:parse markdown))
		   (aref replacements (parse-integer (reg 0))))))
	(write-sequence (clean-html* html) stream)))))

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
	 (all-data (handler-case
		       (sb-sys:with-deadline (:seconds 0.3)
			 (lw2.backend:get-page-body (list-cond
						     (page-alias (cons :page-alias page-alias))
						     (id (cons :lens-id id)))
						    page-type))
		     (serious-condition () nil)))
	 (page-data (cdr (assoc
			  (or id
			      (cdr (assoc :page-id (cdr (assoc :result all-data))))
			      (cdr (assoc :primary-page-id (cdr (assoc :result all-data)))))
			  (cdr (assoc :pages all-data))
			  :test #'string=))))
    (renderer ()
      (unless all-data
	(emit-page (*html-output* :title "Loading" :content-class "loading-page" :return-code 504
				  :extra-head (lambda () (format *html-output* "<meta http-equiv=\"refresh\" content=\"5\">")))
	  <h1>One moment...</h1>
	  <img src="/assets/telegraph.jpg">
	  <p>Loading data from Arbital, please be patient...</p>)
	(return nil))
      (let ((*arbital-context* (cdr (assoc :pages all-data))))
	(emit-page (*html-output* :title (format nil "~:[~;Explore: ~]~A" (eq page-type :explore) (cdr (assoc :title page-data))))
	    <main class="post">
	    <h1 class="post-title">(cdr (assoc :title page-data))</h1>
	    <div class="post-meta">
		(arbital-meta-block page-data all-data :page)
	    </div>
	    (when (and (cdr (assoc :text page-data)) (> (length (cdr (assoc :text page-data))) 0))
	      <div class="body-text post-body">
		(with-html-stream-output
		  (when (assoc :text page-data)
		  (arbital-markdown-to-html (cdr (assoc :text page-data))
					    *html-output*)))
	      </div>)
	    <div class="arbital-nav">
	      (dolist (page-list-data '((:child-ids "Children")
					(:parent-ids "Parents")))
		(destructuring-bind (page-list-id page-list-name) page-list-data
		  (labels
		      ((list-pages (page-list)
			 <ul>
			   (dolist (c page-list)
			     (let ((page-data (cdr (assoc c (cdr (assoc :pages all-data)) :test #'string=))))
			       <li>
				 <a href=("/p/~A~@[?l=~A~]" (cdr (assoc :alias page-data)) c)>(cdr (assoc :title page-data))</a>
				 (with-html-stream-output
				     (when-let (clickbait (cdr (assoc :clickbait page-data)))
					       (arbital-markdown-to-html clickbait *html-output*)))
				 (when-let (page-list (cdr (assoc page-list-id page-data)))
					   (list-pages page-list))
			       </li>))
			 </ul>))
		    (when-let (page-list (cdr (assoc page-list-id page-data)))
			      (unless (eq page-type :explore)
				<p>(progn page-list-name):</p>)
			      (list-pages page-list)))))
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
