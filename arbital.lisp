(uiop:define-package #:3bmd-arbital
  (:use #:cl #:alexandria #:esrap #:3bmd-ext)
  (:export #:*arbital-markdown* #:*arbital-context*))

(in-package #:3bmd-arbital)

(defvar *arbital-context*)

(define-extension-inline *arbital-markdown* arbital-link
  (and "["
       (* (and (! "]") (! " ") character))
       " "
       (* (and (! "]") character))
       (and (! "](") "]"))
  (:destructure (start tag sep text end)
		(declare (ignore start sep end))
		(list :arbital-link (text tag) (text text))))

(defmethod print-tagged-element ((tag (eql :arbital-link)) stream rest)
  (destructuring-bind (tag text) rest
    (if-let (page-alias (cdr (assoc :alias (cdr (assoc (json:json-intern (json:camel-case-to-lisp tag)) *arbital-context*)))))
	    (format stream "<a href=\"/arbital?id=~A&page-alias=~A\">~A</a>" tag page-alias text)
	    (format stream "[unrecognized: ~A ~A]" tag text))))

(define-extension-inline *arbital-markdown* arbital-dollar-sign
  (and "\\$")
  (:constant "$"))

(define-extension-inline *arbital-markdown* arbital-math
  (and "$"
       (* (and (! "$") character))
       "$")
  (:destructure (start text end)
		(declare (ignore start end))
		(list :arbital-math (text text))))

(defmethod print-tagged-element ((tag (eql :arbital-math)) stream rest)
  (destructuring-bind (text) rest
    (format stream "$$~A$$" text)))

(in-package #:lw2-viewer)

(named-readtables:in-readtable html-reader)

(define-component view-arbital-page (id page-alias)
  (:http-args '())
  (let* ((all-data (block nil
		     (loop
			(handler-case
			    (return
			      (json:decode-json-from-string
			       (sb-ext:octets-to-string
				(drakma:http-request "https://arbital.com/json/primaryPage/"
						     :method :post
						     :content (json:encode-json-to-string (alist :page-alias page-alias :lens-id id)))
				:external-format :utf-8)))
			  (t () nil)))))
	 (page-data (cdr (assoc (json:json-intern (json:camel-case-to-lisp (or id page-alias))) (cdr (assoc :pages all-data))))))
    (renderer ()
      (emit-page (*html-output*)
        <main class="post">
          <div class="body-text post-body">
	    (with-html-stream-output
	        (let ((3bmd-arbital:*arbital-markdown* t)
		      (3bmd-arbital:*arbital-context* (cdr (assoc :pages all-data))))
		  (3bmd:parse-string-and-print-to-stream (cdr (assoc :text page-data))
						         *html-output*)))
	    <p>Children:
	      <ul>
	        (dolist (c (cdr (assoc :child-ids page-data)))
	          (let ((page-data (cdr (assoc (json:json-intern (json:camel-case-to-lisp c)) (cdr (assoc :pages all-data))))))
	            <li><a href=("/arbital?id=~A&page-alias=~A" c (cdr (assoc :alias page-data)))>(cdr (assoc :title page-data))</a></li>))
	      </ul>
	    </p>
	  </div>
        </main>))))

(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page nil "84c"))
;(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page "1rf" "probability"))
;(define-route 'arbital-site 'standard-route :name 'view-root :uri "/" :handler (route-component view-arbital-page "7hh" "expected_utility_formalism"))
