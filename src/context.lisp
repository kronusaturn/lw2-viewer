(uiop:define-package #:lw2.context
  (:use #:cl)
  (:export #:*current-site* #:*current-backend* #:*preview* #:*enable-voting*)
  (:recycle #:lw2.context #:lw2.backend #:lw2-viewer))

(in-package #:lw2.context)

(defvar *current-site*)

(defvar *current-backend*)

(defparameter *preview* nil)

(defparameter *enable-voting* nil)
