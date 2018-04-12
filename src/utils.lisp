(uiop:define-package #:lw2.utils
  (:use #:cl)
  (:export #:alist)
  (:recycle #:lw2-viewer))

(in-package #:lw2.utils)

(declaim (inline alist))
(defun alist (&rest parms) (alexandria:plist-alist parms))
