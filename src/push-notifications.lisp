(in-package #:lw2.backend)

(define-cache-database 'backend-push-notifications "push-subscriptions")

(export 'make-subscription)
(defun make-subscription (auth-token endpoint expires)
  (cache-put "push-subscriptions"
	     auth-token
	     (json:encode-json-to-string
	      (alist :endpoint endpoint
		     :expires expires))))

(export 'find-subscription)
(defun find-subscription (auth-token)
  (if-let (json (cache-get "push-subscriptions" auth-token))
	  (json:decode-json-from-string json)))

(export 'delete-subscription)
(defun delete-subscription (auth-token)
  (cache-del "push-subscriptions" auth-token))

(export 'send-all-notifications)
(define-backend-function send-all-notifications ()
  (backend-push-notifications
   (let* ((all-subscriptions
	   (with-collector (col)
	     (call-with-cursor "push-subscriptions"
			       (lambda (db cursor)
				 (declare (ignore db))
				 (multiple-value-bind (value key) (cursor-get cursor :first)
				   (loop while key do
					(col (cons key value))
					(multiple-value-setq (value key) (cursor-get cursor :next)))))
			       :read-only t)
	     (col)))
	  (current-time (local-time:now))
	  (current-time-unix (local-time:timestamp-to-unix current-time)))
     (loop for (auth-token . subscription-json) in all-subscriptions
	do (log-and-ignore-errors
	    (let* ((subscription (json:decode-json-from-string subscription-json))
		   (last-check-cons (or (assoc :last-check subscription) (cons :last-check nil)))
		   (since (if-let (unix (cdr last-check-cons)) (local-time:unix-to-timestamp unix))))
	      (cond
		((> current-time-unix (cdr (assoc :expires subscription)))
		 (delete-subscription auth-token))
		((check-notifications (cache-get "auth-token-to-userid" auth-token) auth-token :since since)
		 (handler-case
		     (send-notification (cdr (assoc :endpoint subscription)))
		   (dex:http-request-gone ()
		     (delete-subscription auth-token))
		   (:no-error (&rest args)
		     (declare (ignore args))
		     (setf (cdr last-check-cons) (local-time:timestamp-to-unix current-time))
		     (cache-put "push-subscriptions" auth-token (json:encode-json-to-string (adjoin last-check-cons subscription))))))))))))
  (backend-base nil))
