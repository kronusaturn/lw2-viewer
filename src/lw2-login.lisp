(defpackage lw2.login
  (:use #:cl #:lw2-viewer.config #:alexandria #:cl-json #:flexi-streams #:websocket-driver-client)
  (:import-from #:ironclad #:byte-array-to-hex-string #:digest-sequence)
  (:export #:do-lw2-login #:do-lw2-create-user #:do-lw2-post #:do-lw2-post-edit #:do-lw2-post-remove #:do-lw2-comment #:do-lw2-comment-edit #:do-lw2-comment-remove))

(in-package #:lw2.login) 

(defparameter *sockjs-debug-output* nil)

(declaim (inline maybe-output))
(defun maybe-output (stream prefix message)
  (if stream (format stream "~&~A: ~A~%" prefix message))
  message)

(defun forwarded-header ()
  (let ((addr (and (boundp 'hunchentoot:*request*) (hunchentoot:real-remote-addr))))
    (if addr
      (list (cons "X-Forwarded-For" addr))
      nil))) 

(defun sockjs-encode-alist (alist)
  (encode-json-to-string (list (encode-json-alist-to-string alist)))) 

(defun sockjs-decode (msg)
  (if (eq (elt msg 0) #\a) 
    (let ((response (map 'list #'decode-json-from-string (decode-json-from-string (subseq msg 1)))))
      (if (= (length response) 1)
	(first response)
	(error "Unsupported sockjs message.")))))

(defun password-digest (password)
  (byte-array-to-hex-string
    (digest-sequence :sha256
      (string-to-octets password :external-format :utf8))))

(defun random-string (length)
  (coerce (loop repeat length collecting (code-char (+ (char-code #\a) (ironclad:strong-random 26)))) 'string)) 

(defun do-lw2-sockjs-operation (operation &optional session)
  (let ((client (wsd:make-client (concatenate 'string *websocket-uri* "sockjs/329/" (random-string 8) "/websocket")
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
					    ("connected" (wsd:send client (maybe-output debug-output "sockjs sent" (sockjs-encode-alist operation))))
					    ("result"
					     (setf result message)
					     (sb-thread:signal-semaphore result-semaphore)))))) 
	(wsd:send client (maybe-output debug-output "sockjs sent" (sockjs-encode-alist `(("msg" . "connect") ,@(if session `(("session" . ,session))) ("version" . "1") ("support" . ("1"))))))
	(sb-thread:wait-on-semaphore result-semaphore :timeout 10))
      (wsd:close-connection client))
    result)) 

(defun parse-login-result (result)
  (let* ((result-inner (cdr (assoc :result result)))
	 (userid (cdr (assoc :id result-inner))) 
	 (token (cdr (assoc :token result-inner))))
    (if (and userid token) 
      (values userid token) 
      (if-let (error-message (cdr (assoc :reason (cdr (assoc :error result)))))
	      (values nil nil error-message)
	      (error "Unknown response from LW2: ~A" result))))) 

(defun do-lw2-login (user-designator-type user-designator password)
  (let ((result (do-lw2-sockjs-operation `(("msg" . "method")
					   ("method" . "login")
					   ("params"
					    (("user" (,user-designator-type . ,user-designator))
					     ("password"
					      (digest . ,(password-digest password))
					      ("algorithm" . "sha-256"))))
					   ("id" . "3")))))
    (parse-login-result result)))

(defun do-lw2-create-user (username email password)
  (let ((result (do-lw2-sockjs-operation `(("msg" . "method")
					   ("method" . "createUser")
					   ("params"
					    (("username" . ,username)
					     ("email" . ,email)
					     ("password"
					      (digest . , (password-digest password))
					      ("algorithm" . "sha-256"))))
					   ("id" . "3")))))
    (parse-login-result result))) 

; resume session ["{\"msg\":\"connect\",\"session\":\"mKvhev8p2f4WfKd6k\",\"version\":\"1\",\"support\":[\"1\",\"pre2\",\"pre1\"]}"]
;
; logout ["{\"msg\":\"method\",\"method\":\"logout\",\"params\":[],\"id\":\"7\"}"]
;
; new user ["{\"msg\":\"method\",\"method\":\"createUser\",\"params\":[{\"username\":\"test2\",\"email\":\"test@example.com\",\"password\":{\"digest\":\"37268335dd6931045bdcdf92623ff819a64244b53d0e746d438797349d4da578\",\"algorithm\":\"sha-256\"}}],\"id\":\"8\"}"]

(defun do-lw2-post-query (auth-token data)
  (let* ((response-json (octets-to-string
			  (drakma:http-request *graphql-uri* :method :post :additional-headers `(("authorization" . ,auth-token)
												 ,@(forwarded-header))
					       :content-type "application/json"
					       :content (encode-json-to-string data))))
	 (response-alist (first (json:decode-json-from-string response-json)))
	 (res-error (first (cdr (assoc :errors response-alist))))
	 (res-data (rest (first (cdr (assoc :data response-alist)))))) 
    (cond
      (res-error (if (search "not_allowed" (cdr (assoc :message res-error))) (error "LW2 server reports: not allowed.")
		   (error "Unknown LW2 error: ~A" res-error)))
      (res-data (cdr (assoc :--id res-data))) 
      (t (error "Unknown response from LW2 server: ~A" response-json))))) 

(defun do-lw2-post (auth-token data)
  (do-lw2-post-query auth-token `((("query" . "mutation PostsNew($document: PostsInput) { PostsNew(document: $document) { __typename, _id, htmlBody } }")
				   ("variables" .
				    (("document" . ,data)))
				   ("operationName" . "PostsNew")))))

(defun do-lw2-post-edit (auth-token post-id set)
  (do-lw2-post-query auth-token `((("query" . "mutation PostsEdit($documentId: String, $set: PostsInput) { PostsEdit(documentId: $documentId, set: $set) { htmlBody } }") ; $unset: PostsUnset
				   ("variables" .
				    (("documentId" . ,post-id)
				     ("set" . ,set)))
				   ("operationName" . "PostsEdit"))))) 

(defun do-lw2-post-remove (auth-token post-id)
  (do-lw2-post-query auth-token `((("query" . "mutation PostsRemove($documentId: String) { PostsRemove(documentId: $documentId) { __typename } }")
				   ("variables" .
				    (("documentId" . ,post-id)))
				   ("operationName" . "PostsRemove")))))

(defun do-lw2-comment (auth-token data)
  (do-lw2-post-query auth-token `((("query" . "mutation CommentsNew ($document: CommentsInput) { CommentsNew (document: $document) { __typename, _id, htmlBody } }")
				   ("variables" .
				    (("document" . ,data)))
				   ("operationName" . "CommentsNew")))))

(defun do-lw2-comment-edit (auth-token comment-id set)
  (do-lw2-post-query auth-token `((("query" . "mutation CommentsEdit($documentId: String, $set: CommentsInput) { CommentsEdit(documentId: $documentId, set: $set) { htmlBody } }")
				   ("variables" .
				    (("documentId" . ,comment-id)
				     ("set" . ,set)))
				   ("operationName" . "CommentsEdit"))))) 

(defun do-lw2-comment-remove (auth-token comment-id)
  (do-lw2-post-query auth-token `((("query" . "mutation CommentsRemove($documentId: String) { CommentsRemove(documentId: $documentId) { __typename } }")
				   ("variables" .
				    (("documentId" . ,comment-id)))
				   ("operationName" . "CommentsRemove")))))
