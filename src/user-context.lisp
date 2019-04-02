(uiop:define-package #:lw2.user-context
  (:use #:cl)
  (:export #:*current-auth-token* #:*current-userid* #:*current-username* #:*current-user-slug* #:*current-ignore-hash*
	   #:logged-in-userid #:logged-in-username #:logged-in-user-slug))

(in-package #:lw2.user-context)

(defvar *current-auth-token*)
(defvar *current-userid*)
(defvar *current-username*)
(defvar *current-user-slug*)
(defvar *current-ignore-hash*)

(defun logged-in-userid (&optional is-userid)
  (let ((current-userid *current-userid*))
    (if is-userid
        (string= current-userid is-userid)
        current-userid))) 

(defun logged-in-username ()
  *current-username*)

(defun logged-in-user-slug ()
  *current-user-slug*)
