(in-package #:lw2.backend)


;;; REST API

(defun do-wl-rest-query (endpoint filters &key auth-token)
  (multiple-value-bind (response-body status-code headers final-uri reuse-stream want-close status-string)
   (drakma:http-request
     (quri:render-uri (quri:merge-uris (quri:make-uri :path endpoint :query filters) (quri:uri (rest-api-uri *current-backend*))))
     :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil))
   (declare (ignore status-code headers final-uri reuse-stream want-close status-string))
   (json:decode-json-from-string (octets-to-string response-body :external-format :utf-8))))

(define-backend-operation get-post-body backend-accordius (post-id &key &allow-other-keys)
  (acons :tags (do-wl-rest-query "tags" `(("document_id" . ,post-id))) (call-next-method)))

;;;; BACKEND SPECIFIC GRAPHQL

(define-backend-operation get-user-posts backend-accordius (user-id &key offset limit (sort-type :date) drafts auth-token)
  (declare (ignore user-id offset limit sort-type drafts auth-token))
  (let ((*graphql-correct* t))
    (declare (special *graphql-correct*))
    (call-next-method)))

(define-backend-operation get-conversation-messages backend-accordius (conversation-id auth-token)
  (declare (ignore conversation-id auth-token))
  (let ((*messages-index-fields* (cons :html-body (remove :content *messages-index-fields*))))
    (call-next-method)))

;;;; LOGIN

(in-package #:lw2.login)

(define-backend-operation do-login backend-accordius (user-designator-type user-designator password)
  (declare (ignore user-designator-type))
  (let* ((response
           (do-lw2-post-query nil `(("query" . "mutation Login($username: String, $password: String) { Login(username: $username, password: $password) {userId, sessionKey, expiration}}")
                                    ("variables" .
                                     (("username" . ,user-designator)
                                      ("password" . ,password))))))
         (user-id (format nil "~A" (cdr (assoc :user-id response))))
         (auth-token (cdr (assoc :session-key response)))
         (expiration (truncate (* 1000 (cdr (assoc :expiration response))))))
    (values user-id auth-token nil expiration)))

(define-backend-operation do-lw2-create-user backend-accordius (username email password)
  ;; TODO: Add actual code
  (let (user-id auth-token error-message expiration)
    (values user-id auth-token error-message expiration)))

(define-backend-operation do-lw2-forgot-password backend-accordius (email)
  ;; TODO: Add actual code
  (let (successfulp error-message)
    (values successfulp error-message)))

(define-backend-operation do-lw2-reset-password backend-accordius (auth-token password)
  ;; TODO: Add actual code
  (let (user-id auth-token error-message expiration)
    (values user-id auth-token error-message expiration)))
