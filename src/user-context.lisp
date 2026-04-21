(uiop:define-package #:lw2.user-context
  (:use #:cl)
  (:export #:*current-auth-token* #:*current-auth-status* #:*current-userid* #:*current-username* #:*current-user-slug* #:*current-ignore-hash* #:*force-human*
	   #:logged-in-userid #:logged-in-username #:logged-in-user-slug #:user-bot-p))

(in-package #:lw2.user-context)

(defvar *current-auth-token*)
(defvar *current-auth-status*)
(defvar *current-userid*)
(defvar *current-username*)
(defvar *current-user-slug*)
(defvar *current-ignore-hash*)
(defvar *force-human* nil)

(defun logged-in-userid (&optional is-userid)
  (let ((current-userid *current-userid*))
    (if is-userid
        (string= current-userid is-userid)
        current-userid))) 

(defun logged-in-username ()
  *current-username*)

(defun logged-in-user-slug ()
  *current-user-slug*)

(defun user-bot-p ()
  (if *force-human*
      nil
      (not (and (boundp '*current-userid) *current-userid*))))
