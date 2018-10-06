(uiop:define-package #:lw2.utils
  (:use #:cl)
  (:export #:alist #:get-unix-time #:substring)
  (:recycle #:lw2-viewer))

(in-package #:lw2.utils)

(declaim (inline alist))
(defun alist (&rest parms) (alexandria:plist-alist parms))

(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(deftype array-dimension-type () `(integer 0 ,(- array-dimension-limit 1)))

(declaim (inline substring)
         (ftype (function (string array-dimension-type &optional array-dimension-type) string) substring))
(defun substring (string start &optional (end (length string)))
  (make-array (- end start) :element-type 'character :displaced-to string :displaced-index-offset start))
