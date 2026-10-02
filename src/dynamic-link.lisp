(uiop:define-package #:lw2.dynamic-link
    (:use #:cl)
  (:import-from #:lw2.utils #:regex-case #:regex-groups-min #:reg)
  (:import-from #:lw2.backend #:user-deleted #:get-slug-userid)
  (:import-from #:lw2.html-reader #:with-html-stream-output)
  (:import-from #:lw2.lmdb #:dynamic-block-original-html)
  (:export #:dynamic-link))

(in-package #:lw2.dynamic-link)

(defun dynamic-link (href attributes inner-html)
  (declare (ignore attributes inner-html))
  (regex-case href
	      ("^/users/([^/#?]+)\\?mention=user"
	       (declare (regex-groups-min 1))
	       (if (user-deleted (get-slug-userid (reg 0)))
		   (with-html-stream-output (:stream stream)
		     (write-string "[deleted user]" stream))
		   (dynamic-block-original-html)))
	      (t
	       (dynamic-block-original-html))))
