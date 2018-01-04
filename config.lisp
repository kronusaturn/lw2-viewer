(defpackage lw2-viewer.config
  (:use #:cl)
  (:export #:*site-uri* #:*graphql-uri* #:*websocket-uri*))
(in-package #:lw2-viewer.config)

(defparameter *site-uri* "http://www.example.com/") 

(defparameter *graphql-uri* "http://198.74.48.181:3000/graphql") 
(defparameter *websocket-uri* "ws://198.74.48.181:3000/") 
