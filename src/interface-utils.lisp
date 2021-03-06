(uiop:define-package #:lw2.interface-utils
  (:use #:cl #:lw2.links #:lw2.html-reader)
  (:export #:pretty-time #:pretty-time-js #:pretty-time-html
	   #:pretty-number #:generate-post-auth-link #:clean-lw-link #:votes-to-tooltip #:vote-buttons))

(in-package #:lw2.interface-utils)

(named-readtables:in-readtable html-reader)

(defun pretty-time (timestring &key format loose-parsing)
  (let ((time (if loose-parsing
		  (chronicity:parse timestring)
		  (local-time:parse-timestring timestring))))
    (values (local-time:format-timestring nil time :timezone local-time:+utc-zone+ :format (or format '(:day #\  :short-month #\  :year #\  :hour #\: (:min 2) #\  :timezone)))
	    (* (local-time:timestamp-to-unix time) 1000))))

(defun pretty-time-js ()
  "<script async src='data:text/javascript,prettyDate()'></script>")

(defun pretty-time-html (timestring &key inline)
  (multiple-value-bind (pretty-time js-time) (pretty-time timestring)
    (format *html-output* "<~A class=\"date hide-until-init\" data-js-date=~A>~A~A</~@*~A>"
	    (if inline "span" "tag")
	    js-time
	    pretty-time
	    (pretty-time-js))))

(defun pretty-number (number &optional object)
  (let ((str (coerce (format nil "~:D~@[<span> ~A~P</span>~]" number object number) '(vector character))))
    (if (eq (aref str 0) #\-)
      (setf (aref str 0) #\MINUS_SIGN))
    str))

(defun generate-post-auth-link (post &optional comment-id absolute need-auth)
  (if need-auth
      (concatenate 'string (generate-item-link :post post :comment-id comment-id :absolute absolute) "?need-auth=y")
      (generate-item-link :post post :comment-id comment-id :absolute absolute)))

(defun clean-lw-link (url)
  (when url
    (ppcre:regex-replace "([^/]*//[^/]*)lesserwrong\.com" url "\\1lesswrong.com")))

(defun votes-to-tooltip (votes)
  (if votes
      (format nil "~A vote~:*~P"
              (typecase votes (integer votes) (list (length votes))))
      ""))

(defun vote-buttons (base-score &key (with-buttons t) vote-count post-id af-score as-text)
  (labels ((button (vote-type)
	     (when with-buttons
	       <button type="button" class=("vote ~A" vote-type) data-vote-type=vote-type data-target-type=(if post-id "Posts" "Comments") tabindex="-1" disabled></button>))
	   (text ()
	     (if (and af-score (/= af-score 0))
		 (format nil "LW: ~A AF: ~A" base-score af-score)
		 (pretty-number base-score "point"))))
    (when base-score
      (if as-text
	  (text)
	  <div class="karma" data-post-id=post-id>
            (button "upvote")
            <span class="karma-value" title=(votes-to-tooltip vote-count)>(safe (text))</span>
            (button "downvote")
          </div>))))
