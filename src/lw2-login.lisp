(uiop:define-package #:lw2.login
  (:use #:cl #:lw2-viewer.config #:lw2.utils #:lw2.graphql #:lw2.backend #:lw2.backend-modules #:alexandria #:cl-json #:flexi-streams #:websocket-driver-client)
  (:import-from #:ironclad #:byte-array-to-hex-string #:digest-sequence)
  (:import-from #:lw2.context #:*current-backend*)
  (:export #:do-lw2-resume #:do-login #:do-lw2-create-user #:do-lw2-forgot-password #:do-lw2-reset-password #:do-logout
	   #:do-lw2-post-query #:do-lw2-post-query*
           #:do-lw2-post #:do-lw2-post-edit #:do-lw2-post-remove #:do-lw2-comment #:do-lw2-comment-edit #:do-lw2-comment-remove
           #:do-lw2-vote #:do-user-edit #:do-create-conversation #:do-create-message)
  (:unintern #:parse-login-result)
  (:recycle #:lw2.utils))

(in-package #:lw2.login) 

(defparameter *sockjs-debug-output* nil)

(declaim (inline maybe-output))
(defun maybe-output (stream prefix message)
  (if stream (format stream "~&~A: ~A~%" prefix message))
  message)

(defun sockjs-encode-alist (alist)
  (encode-json-to-string (list (encode-json-alist-to-string alist)))) 

(defun sockjs-decode (msg)
  (if (eq (elt msg 0) #\a) 
    (let ((response (map 'list #'decode-json-from-string (decode-json-from-string (subseq msg 1)))))
      (if (= (length response) 1)
	(first response)
	(error "Unsupported sockjs message.")))))

(defun password-digest (password &key (algorithm :sha256))
  (byte-array-to-hex-string
    (digest-sequence algorithm
      (string-to-octets password :external-format :utf8))))

(defun do-lw2-sockjs-operation (operation)
  (let ((client (wsd:make-client (concatenate 'string (websocket-uri *current-backend*) "sockjs/329/" (random-string 8) "/websocket")
				 :additional-headers (forwarded-header)))
	(debug-output *sockjs-debug-output*) 
	(result-semaphore (sb-thread:make-semaphore))
	result)
    (unwind-protect
      (progn
	(wsd:start-connection client) 
	(wsd:on :message client (lambda (encoded-message)
				  (maybe-output debug-output "sockjs recd" encoded-message)
				  (let ((message (sockjs-decode encoded-message)))
				    (switch ((cdr (assoc :msg message)) :test 'equal)
					    ("connected"
                                             (wsd:send client (maybe-output debug-output "sockjs sent" (sockjs-encode-alist operation))))
					    ("result"
					     (setf result message)
					     (sb-thread:signal-semaphore result-semaphore)))))) 
	(wsd:send client (maybe-output debug-output "sockjs sent" (sockjs-encode-alist (alist "msg" "connect"
											      "version" "1"
											      "support" '("1")))))
	(unless (sb-thread:wait-on-semaphore result-semaphore :timeout 10)
          (error "Timeout while waiting for LW2 server.")))
      (wsd:close-connection client))
    result))

(defun do-lw2-sockjs-method (method &rest params)
  (do-lw2-sockjs-operation (alist :msg "method"
				  :method method
				  :params params
				  :id "3")))

(defun parse-websocket-login-result (result)
  (let* ((result-inner (cdr (assoc :result result)))
	 (userid (cdr (assoc :id result-inner))) 
	 (token (cdr (assoc :token result-inner)))
         (expires (cdadr (assoc :token-expires result-inner))))
    (if (and userid token)
      (values userid token nil (and expires (floor expires 1000)))
      (if-let (error-message (cdr (assoc :reason (cdr (assoc :error result)))))
	      (values nil nil error-message)
	      (error "Unknown response from LW2: ~A" result)))))

(defun do-graphql-post-query (auth-token data)
  (call-with-http-response
   #'json:decode-json
   (graphql-uri *current-backend*)
   :method :post
   :want-stream t
   :headers (backend-request-headers auth-token t)
   :content (encode-json-to-string data)))

(defun do-lw2-resume (auth-token)
  (let ((result (do-lw2-sockjs-method "login" (alist :resume auth-token))))
    (parse-websocket-login-result result)))

(define-backend-function do-login (user-designator password &key (try-legacy t))
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "login"
		   (alist :user (alist "username" user-designator)
			  :password (alist :digest (password-digest password)
					   :algorithm "sha-256")))))
     (trivia:match result
		   ((assoc :error (trivia:alist (:error . "legacy-account")
						(:details . (trivia:alist (:salt . legacy-salt)
									  (:username . legacy-username)))))
		    (if try-legacy
			(do-login user-designator
				  (format nil "~A~A" legacy-salt
					  (password-digest (format nil "~A~A ~A" legacy-salt legacy-username password)
							   :algorithm :sha1))
				  :try-legacy nil)
			(values nil nil "Incorrect password")))
		   (_
		    (parse-websocket-login-result result))))))

(define-backend-function do-lw2-create-user (username email password)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "createUser"
		   (alist :username username
			  :email email
			  :password (alist :digest (password-digest password)
					   :algorithm "sha-256")))))
     (parse-websocket-login-result result))))

(define-backend-function do-lw2-forgot-password (email)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "forgotPassword"
		   (alist :email email))))
     (if-let (error-data (cdr (assoc :error result)))
	     (values nil (cdr (assoc :reason error-data)))
	     t))))

(define-backend-function do-lw2-reset-password (auth-token password)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "resetPassword"
		   auth-token
		   (alist :digest (password-digest password)
			  :algorithm "sha-256"))))
     (parse-websocket-login-result result))))

(define-backend-function do-logout (auth-token)
  (backend-websocket-login
   (declare (ignore auth-token))))

(defun parse-passport-js-login-result (result)
  (let* ((res-errors (first (cdr (assoc :errors result))))
	 (res-data (cdr (first (cdr (assoc :data result)))))
	 (token (cdr (assoc :token res-data))))
    (if res-errors
	(values nil nil (cdr (assoc :message res-errors)))
	(let ((user-id (cdr (first (do-lw2-post-query
				       token (alist "query"
						    (graphql-query-string :current-user nil '(:--id))))))))
	  (values user-id token nil)))))

(defun do-passport-js-login-operation (operation params)
  (parse-passport-js-login-result
   (do-graphql-post-query nil (alist :query (graphql-operation-string :mutation operation params '(:token))))))

(define-backend-operation do-login backend-passport-js-login (user-designator password &key try-legacy)
  (declare (ignore try-legacy))
  (do-passport-js-login-operation :login (alist :username user-designator
						:password password)))

(define-backend-operation do-lw2-create-user backend-passport-js-login (username email password)
  (do-passport-js-login-operation :signup (alist :username username
						 :email email
						 :password password)))

(define-backend-operation do-logout backend-passport-js-login (auth-token)
  (do-graphql-post-query auth-token (alist :query (graphql-operation-string :mutation :logout nil '(:token)))))

; (do-lw2-post-query "OCP7NeJEW9fPpYGG_nCN3g0felGTTNd0eg5uiLNQqBR" `((("query" . "mutation vote($documentId: String, $voteType: String, $collectionName: String) { vote(documentId: $documentId, voteType: $voteType, collectionName: $collectionName) { ... on Post { currentUserVotes { _id, voteType, power } } } }") ("variables" ("documentId" . "sqhAntEGpYgFXXH2H") ("voteType" . "upvote") ("collectionName" . "Posts")) ("operationName" . "vote"))))

(defun do-lw2-post-query (auth-token data)
  (lw2.backend::do-graphql-debug data)
  (let* ((response-alist (do-graphql-post-query auth-token data))
	 (res-errors (cdr (assoc :errors response-alist)))
	 (res-data (rest (first (cdr (assoc :data response-alist))))))
    (cond
      (res-errors (lw2.backend:signal-lw2-errors res-errors))
      (res-data res-data) 
      (t (error "Unknown response from LW2 server: ~A" response-alist)))))

(defun do-lw2-post-query* (auth-token data)
  (cdr (assoc :--id (do-lw2-post-query auth-token data))))

(define-backend-function lw2-mutation-string (target-type mutation-type terms fields)
  (backend-lw2-legacy
   (let* ((mutation-type-string (case mutation-type
				  (:create "New")
				  (:update "Edit")
				  (:delete "Remove")))
	  (mutation-name (concatenate 'string
				      (if (eq target-type :user)
					  (string-downcase target-type)
					  (string-capitalize target-type))
				      "s" mutation-type-string)))
     (values (graphql-mutation-string mutation-name terms fields) mutation-name)))
  (backend-lw2-modernized
   (let* ((mutation-name (concatenate 'string (string-downcase mutation-type) (string-capitalize target-type)))
	  (selector-type (concatenate 'string (string-capitalize target-type) "SelectorUniqueInput"))
	  (data-type (concatenate 'string (string-capitalize mutation-type) (string-capitalize target-type) "DataInput"))
	  (data (append
		 (cdr (assoc :document terms))
		 (cdr (assoc :set terms))
		 (map 'list (lambda (x) (cons (car x) :null)) (cdr (assoc :unset terms)))))
	  (data (map 'list (lambda (x) (destructuring-bind (k . v) x
					 (if (eq k :body)
					     (cons :contents
						   (alist :original-contents (alist :data v :type "markdown")
							  :update-type "minor"
							  :commit-message ""))
					     x)))
		     data))
	  (terms (nconc
		  (loop for (k . v) in terms nconc
		       (case k
			 (:document nil)
			 (:set nil)
			 (:unset nil)
			 (:document-id (alist :selector (alist :document-id v)))
			 (t (list (cons k v)))))
		  (when data
		    (nalist :data data)))))
     (values (with-output-to-string (stream)
	       (format stream "mutation ~A(~@[$selector: ~A!, ~]$data: ~A!)~3:*{~A(~:[~;selector: $selector, ~]data: $data)"
		     mutation-name
		     (if (cdr (assoc :selector terms)) selector-type)
		     data-type)
	       (write-graphql-simple-field-list (list (list* :data fields)) stream)
	       (write-string "}" stream))
	     mutation-name
	     terms))))
#|
do-lw2-mutation:

Low level graphQL mutation string builder. This is a good function to override 
with a define-backend-operation and translate into the semantics of higher level functions,
that way you can avoid having to recreate them all for each backend service.

auth-token - The authentication token to use with the API, fairly straightforward.
target-type - The type of object that we're mutating
mutation-type - The request method (or analogous equivalent) that we're using
terms - The additional parameters/variables/etc that we're sending to the server with our request
fields - The return values we want to get from the server after it completes our request
|#
(define-backend-function do-lw2-mutation (auth-token target-type mutation-type terms fields)
  (backend-lw2-legacy
   (multiple-value-bind (mutation-string operation-name variables)
       (lw2-mutation-string target-type mutation-type terms fields)
     (do-lw2-post-query auth-token (alist "query" mutation-string
					  "variables" variables
					  "operationName" operation-name))))
  (backend-lw2-modernized
   (cdr (assoc :data (call-next-method)))))

(defun do-lw2-post (auth-token data)
  (do-lw2-mutation auth-token :post :create (alist :document data) '(:--id :slug :html-body)))

(defun do-lw2-post-edit (auth-token post-id set &optional unset)
  (let* ((terms (alist* :document-id post-id :set set
			(alist-without-null :unset unset))))
    (declare (dynamic-extent terms))
    (do-lw2-mutation auth-token :post :update terms '(:--id :slug :html-body))))

(defun do-lw2-post-remove (auth-token post-id)
  (do-lw2-mutation auth-token :post :delete (alist :document-id post-id) '(:----typename)))

(defun do-lw2-comment (auth-token data)
  (do-lw2-mutation auth-token :comment :create (alist :document data) '(:--id :html-body)))

(defun do-lw2-comment-edit (auth-token comment-id set)
  (do-lw2-mutation auth-token :comment :update (alist :document-id comment-id :set set) '(:--id :html-body)))

(define-backend-function do-lw2-comment-remove (auth-token comment-id &key reason)
  (backend-lw2-legacy
   (declare (ignore reason)) ; reasons not supported
   (do-lw2-mutation auth-token :comment :delete (alist :document-id comment-id) '(----typename)))
  (backend-lw2-modernized
   (do-lw2-comment-edit auth-token comment-id (alist :deleted t :deleted-public t
						     :deleted-reason reason))))

(defun do-lw2-vote (auth-token target-collection target-id vote)
  (let* ((mutation (format nil "setVote~:(~A~)" target-collection))
	 (karma-vote (or (nonempty-string vote)
			 (cdr (assoc :karma vote))))
	 (extended-vote (remove :karma vote :key #'car))
	 (ret (do-lw2-post-query auth-token
		(alist "query" (graphql-mutation-string mutation
							(remove-if #'null
								   (alist :document-id target-id
									  :vote-type karma-vote
									  :extended-vote extended-vote)
								   :key #'cdr)
							'(:--id :base-score :af :af-base-score :vote-count :extended-score
							  :current-user-vote :current-user-extended-vote))
		       "variables" nil
		       "operationName" mutation)))
	 (confirmed-vote (block nil
			   (alist-bind (current-user-vote current-user-extended-vote) ret
				       (return (list-cond* (current-user-vote :karma current-user-vote)
							   current-user-extended-vote))))))
    (values confirmed-vote ret)))
    

(defun do-user-edit (auth-token user-id data)
  (do-lw2-mutation auth-token :user :update (alist :document-id user-id :set data) '(--id)))

(define-backend-function do-create-conversation (auth-token data)
  (backend-lw2-legacy
   (cdr (assoc :--id (do-lw2-mutation auth-token :conversation :create (alist :document data) '(:--id))))))

(define-backend-function generate-message-document (conversation-id text)
  (backend-lw2-legacy
   (alist :content
	  (alist :blocks (loop for para in (ppcre:split "\\n+" text)
			    collect (alist :text para :type "unstyled"))
		 :entity-map (make-hash-table))
	  :conversation-id conversation-id))
  (backend-lw2-modernized
   (alist :body text
	  :conversation-id conversation-id)))

(define-backend-function do-create-message (auth-token conversation-id text)
  (backend-lw2-legacy
   (do-lw2-mutation auth-token :message :create (alist :document (generate-message-document conversation-id text)) '(:--id))))
