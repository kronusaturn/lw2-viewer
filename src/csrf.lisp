(uiop:define-package #:lw2.csrf
  (:use #:cl #:lw2.conditions #:lw2.client-script)
  (:export #:make-csrf-token #:check-csrf-token #:check-csrf)
  (:recycle #:lw2-viewer))

(in-package #:lw2.csrf)

(client-defun make-csrf-token (&optional (session-token (when-server (hunchentoot:cookie-in "session-token"))) (nonce (when-server (ironclad:make-random-salt))))
  (if-client
   (ps:chain -g-w csrf-token)
   (progn
     (if (typep session-token 'string) (setf session-token (base64:base64-string-to-usb8-array session-token)))
     (let ((csrf-token (concatenate '(vector (unsigned-byte 8)) nonce (ironclad:digest-sequence :sha256 (concatenate '(vector (unsigned-byte 8)) nonce session-token)))))
       (values (base64:usb8-array-to-base64-string csrf-token) csrf-token)))))

(defun check-csrf-token (csrf-token &optional (session-token (hunchentoot:cookie-in "session-token")))
  (unless (and (> (length csrf-token) 0)
	       (> (length session-token) 0))
    (error 'csrf-check-failed))
  (let* ((session-token (base64:base64-string-to-usb8-array session-token))
	 (csrf-token (base64:base64-string-to-usb8-array csrf-token))
	 (correct-token (nth-value 1 (make-csrf-token session-token (subseq csrf-token 0 16)))))
    (unless (ironclad:constant-time-equal csrf-token correct-token)
      (error 'csrf-check-failed))
    t)) 

(defun check-csrf ()
  (unless (member (hunchentoot:request-method*) '(:get :head))
    (check-csrf-token (hunchentoot:post-parameter "csrf-token"))))
