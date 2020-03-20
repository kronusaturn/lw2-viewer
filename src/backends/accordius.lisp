(in-package #:lw2.backend)

;;; REST API

(defun do-wl-rest-query (endpoint filters &key auth-token)
  (lw2-graphql-query (lambda () (values endpoint filters)) :auth-token auth-token))

(defclass accordius-query (closer-mop:funcallable-standard-object) ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod run-query ((query accordius-query))
  (lw2-graphql-query query :return-type :string))

(define-backend-operation lw2-query-string backend-accordius (query-type return-type args &rest rest)
  (declare (ignore rest))
  (let ((obj (make-instance 'accordius-query)))
    (closer-mop:set-funcallable-instance-function
     obj
     (lambda ()
       (values
	(format nil "~(~A~)s/~@[~A/~]" query-type (if (eq return-type :single) (cdr (assoc :document-id args))))
	(loop for arg in args
	   unless (member (car arg) '(:document-id :limit :view))
	   collect (cons (json:lisp-to-camel-case (string (car arg))) (cdr arg))))))
    obj))

(define-backend-operation postprocess-query-result backend-accordius (result)
  (if-let (data (assoc :data result))
	  (cdadr data)
	  result))

(define-backend-operation call-with-backend-response backend-accordius (fn query &key auth-token)
   (multiple-value-bind (endpoint filters)
       (funcall query)
     (call-with-http-response
      fn
      (quri:render-uri (quri:merge-uris (quri:make-uri :path endpoint :query filters) (quri:uri (rest-api-uri *current-backend*))))
      :additional-headers (if auth-token `(("authorization" . ,auth-token)) nil)
      :want-stream t)))

(define-backend-operation get-post-body backend-accordius (post-id &key &allow-other-keys)
  (acons :tags (lw2-graphql-query (lambda () (values "tags/" `(("document_id" . ,post-id))))) (call-next-method)))

(define-backend-operation lw2-search-query backend-accordius (query)
  (values
   (do-wl-rest-query "post_search/" `(("query" . ,query)))
   (do-wl-rest-query "comment_search/" `(("query" . ,query)))))

(defun do-wl-rest-mutate (mutation-type endpoint post-params auth-token)
  (drakma:http-request
   (quri:render-uri (quri:merge-uris (quri:make-uri :path endpoint :query "") (quri:uri (rest-api-uri *current-backend*))))
   :method mutation-type
   :parameters post-params
   :additional-headers `(("authorization" . ,auth-token)))
  )

(defun do-wl-create-tag (document-id text auth-token)
  (do-wl-rest-mutate :post "tags/" `((:DOCUMENT-ID . ,document-id) (:TEXT . ,text)) auth-token))
  

;;;; BACKEND SPECIFIC GRAPHQL

(define-backend-operation get-user-page-items backend-accordius (user-id request-type &key &allow-other-keys)
  (declare (ignore user-id request-type))
  (let ((*graphql-correct* t))
    (declare (special *graphql-correct*))
    (call-next-method)))

(define-backend-operation get-conversation-messages backend-accordius (conversation-id auth-token)
  (declare (ignore conversation-id auth-token))
  (let ((*messages-index-fields* (cons :html-body (remove :content *messages-index-fields*))))
    (call-next-method)))

(define-backend-operation user-fields backend-accordius ()
  (remove :groups (call-next-method)))

;;;; LOGIN

(in-package #:lw2.login)

(define-backend-operation do-lw2-mutation backend-accordius (auth-token target-type mutation-type terms fields)
  (let ((endpoint
	 (case target-type
	   (:post "posts")
	   (:comment "comments")
	   )))
    (cond
      ((eq mutation-type :delete) (do-wl-rest-mutate mutation-type
				    (concatenate 'string endpoint "/"
						 (cdr (assoc :DOCUMENT-ID terms)))
				    nil
				    auth-token))
      (t (call-next-method)))))
   

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
