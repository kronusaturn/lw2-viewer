(uiop:define-package #:lw2.context
  (:use #:cl)
  (:export #:*current-site* #:*current-backend*)
  (:recycle #:lw2.backend))

(in-package #:lw2.context)

(defvar *current-site*)

(defvar *current-backend*)
