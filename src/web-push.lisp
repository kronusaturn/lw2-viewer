(uiop:define-package #:lw2.web-push
  (:use #:cl #:alexandria #:lw2.utils #:lw2.conditions)
  (:export #:get-vapid-public-key #:send-notification))

(in-package #:lw2.web-push)

(defparameter *vapid-key-filename* (asdf:system-relative-pathname :lw2-viewer "webpush.vapid.key"))
(sb-ext:defglobal *vapid-key* nil)
(sb-ext:defglobal *vapid-header-cache* (make-hash-table :test 'equal))
(sb-ext:defglobal *vapid-header-cache-lock* (sb-thread:make-mutex :name "vapid header cache lock"))

(defun call-with-private-file (fn name)
  (let ((fd nil))
    (unwind-protect
	 (progn
	   (setf fd (sb-posix:open name (logior sb-posix:o-creat sb-posix:o-rdwr) #o600))
	   (funcall fn (sb-sys:make-fd-stream fd :input t :output t)))
      (when fd (sb-posix:close fd)))))

(defun invoke-node-process (command &optional (output #'json:decode-json))
  (uiop:run-program "node js-foreign-lib/web-push.js"
		    :input (list command)
		    :output output
		    :error-output *error-output*))

(defun ensure-vapid-key ()
  (unless *vapid-key*
    (labels ((read-vapid-key (stream)
	       (setf *vapid-key* (json:decode-json stream))))
      (log-and-ignore-errors
       (with-open-file (stream *vapid-key-filename* :direction :input :if-does-not-exist nil)
	 (if stream
	     (read-vapid-key stream)
	     (call-with-private-file (lambda (stream)
				       (invoke-node-process "webPush.generateVAPIDKeys()" stream)
				       (file-position stream 0)
				       (read-vapid-key stream))
				     *vapid-key-filename*)))))))

(ensure-vapid-key)

(defun get-vapid-public-key ()
  (cdr (assoc :public-key *vapid-key*)))

(defun generate-vapid-headers (origin)
  (let* ((result-json
	  (invoke-node-process
	   (format nil "webPush.getVapidHeaders(~{~A~^,~});"
		   (mapcar #'json:encode-json-to-string
			   (list origin "mailto:test@example.com"
				 (cdr (assoc :public-key *vapid-key*)) (cdr (assoc :private-key *vapid-key*)) "aes128gcm")))))
	 (result-string (cdar result-json))
	 (result-parts (nth-value 1
				  (ppcre:scan-to-strings "vapid t=([^,]+), k=([^,]+)" result-string))))
    (assert (= (length result-parts) 2))
    (alist :authorization (format nil "WebPush ~A" (aref result-parts 0))
	   :crypto-key (format nil "p256ecdsa=~A" (aref result-parts 1)))))

(defun get-vapid-headers (origin)
  (let ((unlocked-value
	 (sb-thread:with-mutex (*vapid-header-cache-lock*)
	   (let ((value (gethash origin *vapid-header-cache*))
		 (current-time (get-unix-time)))
	     (if (and value (< current-time (+ (car value) (* 60 60 12))))
		 (cdr value)
		 (setf (gethash origin *vapid-header-cache*)
		       (sb-thread:make-thread (lambda ()
						(let ((vapid (generate-vapid-headers origin)))
						  (sb-thread:with-mutex (*vapid-header-cache-lock*)
						    (setf (gethash origin *vapid-header-cache*) (cons current-time vapid)))
						  vapid))
					      :name "generate vapid headers")))))))
    (typecase unlocked-value
      (sb-thread:thread (sb-thread:join-thread unlocked-value))
      (t unlocked-value))))

(defun send-notification (endpoint &key (ttl (* 60 60 24)))
  ;; we don't support content yet since it requires encryption
  (dex:request endpoint
	       :method :post
	       :headers (list* (cons :ttl ttl)
			       (cons :content-encoding "identity")
			       (get-vapid-headers (quri:render-uri (quri:merge-uris "/" endpoint))))
	       :keep-alive nil))
