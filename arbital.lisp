(uiop:define-package #:3bmd-arbital
  (:use #:cl #:alexandria #:esrap #:3bmd-ext)
  (:export #:*arbital-markdown* #:*arbital-context*))

(in-package #:3bmd-arbital)

(defvar *arbital-context*)

(define-extension-inline *arbital-markdown* arbital-link
  (and "["
       (* (and (! "]") (! " ") character))
       (? (and
	   " "
	   (* (and (! "]") character))))
       (and "]" (! "(")))
  (:destructure (start tag (sep text) end)
		(declare (ignore start sep end))
		(list :arbital-link (text tag) (text text))))

(defmethod print-tagged-element ((tag (eql :arbital-link)) stream rest)
  (destructuring-bind (tag text) rest
    (cond
      ((ppcre:scan "^http" tag)
       (format stream "<a href=\"~A\">~A</a>" tag text))
      ((ppcre:scan ":$" tag)
       nil)
      (t
       (if-let (page-alias (cdr (assoc :alias (cdr (assoc tag *arbital-context* :test #'string=)))))
	       (format stream "<a href=\"/p/~A~@[?l=~A~]\">~A</a>" page-alias tag text)
	       (format stream "<span class=\"redlink\" title=\"~A\">~A</span>" tag text))))))

(define-extension-inline *arbital-markdown* arbital-dollar-sign
  (and "\\$")
  (:constant "$"))

(define-extension-inline *arbital-markdown* arbital-math
  (and "$"
       (and (! "$")
	    (* (or "\\$"
		   (and (! "$") character))))
       "$")
  (:destructure (start text end)
		(declare (ignore start end))
		(list :arbital-math (text text))))

(defmethod print-tagged-element ((tag (eql :arbital-math)) stream rest)
  (destructuring-bind (text) rest
    (format stream "\\(~A\\)" text)))

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
	          (let ((3bmd-arbital:*arbital-markdown* t)
		        (3bmd-arbital:*arbital-context* (cdr (assoc :pages all-data))))
		    (3bmd:parse-string-and-print-to-stream (cdr (assoc :text page-data))
						           *html-output*))))
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
        </main>))))

(define-route 'arbital-site 'standard-route :name 'view-arbital-root :uri "/" :handler (route-component view-arbital-page () nil "84c" :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-page :regex "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :primary-page))
(define-route 'arbital-site 'regex-route :name 'view-arbital-explore :regex "/explore/([^/]+)" :handler (route-component view-arbital-page (page-alias) nil page-alias :explore))
;(define-route 'arbital-site 'regex-route :name 'view-root :uri "/p/([^/]+)" :handler (route-component view-arbital-page (page-alias) "1rf" "probability"))
;(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page "7hh" "expected_utility_formalism"))
