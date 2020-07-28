(uiop:define-package #:lw2.login
  (:use #:cl #:lw2-viewer.config #:lw2.utils #:lw2.graphql #:lw2.backend #:lw2.backend-modules #:alexandria #:cl-json #:flexi-streams #:websocket-driver-client)
  (:import-from #:ironclad #:byte-array-to-hex-string #:digest-sequence)
  (:import-from #:lw2.context #:*current-backend*)
  (:export #:do-lw2-resume #:do-login #:do-lw2-create-user #:do-lw2-forgot-password #:do-lw2-reset-password
	   #:do-lw2-post-query #:do-lw2-post-query*
           #:do-lw2-post #:do-lw2-post-edit #:do-lw2-post-remove #:do-lw2-comment #:do-lw2-comment-edit #:do-lw2-comment-remove
           #:do-lw2-vote #:do-user-edit #:do-create-conversation #:do-create-message))

(in-package #:lw2.login) 

(defparameter *sockjs-debug-output* nil)

(declaim (inline maybe-output))
(defun maybe-output (stream prefix message)
  (if stream (format stream "~&~A: ~A~%" prefix message))
  message)

(defun forwarded-header ()
  (let ((addr (and (boundp 'hunchentoot:*request*) (hunchentoot:real-remote-addr))))
    (list-cond (addr "X-Forwarded-For" addr))))

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

(defun random-string (length)
  (coerce (loop repeat length collecting (code-char (+ (char-code #\a) (ironclad:strong-random 26)))) 'string)) 

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
	(wsd:send client (maybe-output debug-output "sockjs sent" (sockjs-encode-alist `(("msg" . "connect") ("version" . "1") ("support" . ("1"))))))
        (unless (sb-thread:wait-on-semaphore result-semaphore :timeout 10)
          (error "Timeout while waiting for LW2 server.")))
      (wsd:close-connection client))
    result))

(defun do-lw2-sockjs-method (method params)
  (do-lw2-sockjs-operation `(("msg" . "method")
                             ("method" . ,method)
                             ("params" . ,params)
                             ("id" . "3"))))

(defun parse-login-result (result)
  (let* ((result-inner (cdr (assoc :result result)))
	 (userid (cdr (assoc :id result-inner))) 
	 (token (cdr (assoc :token result-inner)))
         (expires (cdadr (assoc :token-expires result-inner))))
    (if (and userid token)
      (values userid token nil (and expires (floor expires 1000)))
      (if-let (error-message (cdr (assoc :reason (cdr (assoc :error result)))))
	      (values nil nil error-message)
	      (error "Unknown response from LW2: ~A" result)))))

(defun do-lw2-resume (auth-token)
  (let ((result (do-lw2-sockjs-method "login" `((("resume" . ,auth-token))))))
    (parse-login-result result)))

(define-backend-function do-login (user-designator-type user-designator password &key (try-legacy t))
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "login"
		   `((("user" (,user-designator-type . ,user-designator))
		      ("password"
		       (digest . ,(password-digest password))
		       ("algorithm" . "sha-256")))))))
     (trivia:match result
		   ((assoc :error (trivia:alist (:error . "legacy-account")
						(:details . (trivia:alist (:salt . legacy-salt)
									  (:username . legacy-username)))))
		    (if try-legacy
			(do-login user-designator-type user-designator
				  (format nil "~A~A" legacy-salt
					  (password-digest (format nil "~A~A ~A" legacy-salt legacy-username password)
							   :algorithm :sha1))
				  :try-legacy nil)
			(values nil nil "Incorrect password")))
		   (_
		    (parse-login-result result))))))

(define-backend-function do-lw2-create-user (username email password)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "createUser"
		   `((("username" . ,username)
		      ("email" . ,email)
		      ("password"
		       (digest . ,(password-digest password))
		       ("algorithm" . "sha-256")))))))
     (parse-login-result result)))) 

(define-backend-function do-lw2-forgot-password (email)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "forgotPassword"
		   `((("email" . ,email))))))
     (if-let (error-data (cdr (assoc :error result)))
	     (values nil (cdr (assoc :reason error-data)))
	     t))))

(define-backend-function do-lw2-reset-password (auth-token password)
  (backend-websocket-login
   (let ((result (do-lw2-sockjs-method "resetPassword"
		   `(,auth-token
		     ((digest . ,(password-digest password))
		      ("algorithm" . "sha-256"))))))
     (parse-login-result result))))

; resume session ["{\"msg\":\"connect\",\"session\":\"mKvhev8p2f4WfKd6k\",\"version\":\"1\",\"support\":[\"1\",\"pre2\",\"pre1\"]}"]
;
; logout ["{\"msg\":\"method\",\"method\":\"logout\",\"params\":[],\"id\":\"7\"}"]
;
; new user ["{\"msg\":\"method\",\"method\":\"createUser\",\"params\":[{\"username\":\"test2\",\"email\":\"test@example.com\",\"password\":{\"digest\":\"37268335dd6931045bdcdf92623ff819a64244b53d0e746d438797349d4da578\",\"algorithm\":\"sha-256\"}}],\"id\":\"8\"}"]

; (do-lw2-post-query "OCP7NeJEW9fPpYGG_nCN3g0felGTTNd0eg5uiLNQqBR" `((("query" . "mutation vote($documentId: String, $voteType: String, $collectionName: String) { vote(documentId: $documentId, voteType: $voteType, collectionName: $collectionName) { ... on Post { currentUserVotes { _id, voteType, power } } } }") ("variables" ("documentId" . "sqhAntEGpYgFXXH2H") ("voteType" . "upvote") ("collectionName" . "Posts")) ("operationName" . "vote"))))

(defun do-lw2-post-query (auth-token data)
  (lw2.backend::do-graphql-debug data)
  (let* ((response-json
	  (call-with-http-response
	   #'identity
	   (graphql-uri *current-backend*)
	   :method :post
	   :headers (nconc (list-cond (t "Content-Type" "application/json")
				      (auth-token "authorization" auth-token))
			   (forwarded-header))
	   :content (encode-json-to-string data)))
	 (response-alist (json:decode-json-from-string response-json))
	 (res-errors (cdr (assoc :errors response-alist)))
	 (res-data (rest (first (cdr (assoc :data response-alist)))))) 
    (cond
      (res-errors (lw2.backend:signal-lw2-errors res-errors))
      (res-data res-data) 
      (t (error "Unknown response from LW2 server: ~A" response-json))))) 

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
						   (alist :update-type "minor"
							  :commit-message ""
							  :original-contents (alist :type "markdown" :data v)))
					     x)))
		     data))
	  (terms (nconc
		  (loop for (k . v) in terms nconc
		       (case k
			 (:document nil)
			 (:set nil)
			 (:unset nil)
			 (:document-id (list (cons :selector (alist :document-id v))))
			 (t (list (cons k v)))))
		  (when data
		    (list (cons :data data))))))
     (values (format nil "mutation ~A(~@[$selector: ~A!, ~]$data: ~A!)~3:*{~A(~:[~;selector: $selector, ~]data: $data)~*{data{~{~A~^, ~}}}}"
		     mutation-name
		     (if (cdr (assoc :selector terms)) selector-type)
		     data-type
		     (map 'list (lambda (x) (json:lisp-to-camel-case (string x))) fields))
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
     (do-lw2-post-query auth-token `(("query" . ,mutation-string)
				     ("variables" . ,variables)
				     ("operationName" . ,operation-name)))))
  (backend-lw2-modernized
   (cdr (assoc :data (call-next-method)))))

(defun do-lw2-post (auth-token data)
  (do-lw2-mutation auth-token :post :create (alist :document data) '(:--id :slug :html-body)))

(defun do-lw2-post-edit (auth-token post-id set &optional unset)
  (let* ((terms (alist :document-id post-id :set set))
         (terms (if unset (acons :unset unset terms) terms)))
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

(defun do-lw2-vote (auth-token target target-type vote-type)
  (let ((ret (do-lw2-post-query auth-token
	       `(("query" . "mutation vote($documentId: String, $voteType: String, $collectionName: String) { vote(documentId: $documentId, voteType: $voteType, collectionName: $collectionName) { ... on Post { baseScore, af, afBaseScore, currentUserVotes { _id, voteType, power } } ... on Comment { baseScore, af, afBaseScore, currentUserVotes { _id, voteType, power } } } }")
		  ("variables" ("documentId" . ,target) ("voteType" . ,vote-type) ("collectionName" . ,target-type)) ("operationName" . "vote")))))
    (values (cdr (assoc :base-score ret)) (cdr (assoc :vote-type (first (cdr (assoc :current-user-votes ret))))) ret)))

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
