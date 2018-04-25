(uiop:define-package #:lw2.utils
  (:use #:cl)
  (:export #:alist #:get-unix-time)
  (:recycle #:lw2-viewer))

(in-package #:lw2.utils)

(declaim (inline alist))
(defun alist (&rest parms) (alexandria:plist-alist parms))

(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))
