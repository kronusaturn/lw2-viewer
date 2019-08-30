(uiop:define-package #:lw2.interface-utils
  (:use #:cl #:lw2.links #:lw2.html-reader)
  (:export #:pretty-time #:pretty-number #:generate-post-auth-link #:clean-lw-link #:votes-to-tooltip #:vote-buttons))

(in-package #:lw2.interface-utils)

(named-readtables:in-readtable html-reader)

(defun pretty-time (timestring &key format loose-parsing)
  (let ((time (if loose-parsing
		  (chronicity:parse timestring)
		  (local-time:parse-timestring timestring))))
  (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	  (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-number (number &optional object)
  (let ((str (coerce (format nil "~:D~@[<span> ~A~P</span>~]" number object number) '(vector character))))
    (if (eq (aref str 0) #\-)
      (setf (aref str 0) #\MINUS_SIGN))
    str))

(defun generate-post-auth-link (post &optional comment-id absolute need-auth)
  (if need-auth
      (concatenate 'string (generate-post-link post comment-id absolute) "?need-auth=y")
      (generate-post-link post comment-id absolute)))

(defun clean-lw-link (url)
  (when url
    (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com")))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun vote-buttons (base-score &key vote-count post-id)
  <div class="karma" data-post-id=post-id>
    <span class="karma-value" title=(votes-to-tooltip vote-count)>(safe (pretty-number base-score "point"))</span>
  </div>)
