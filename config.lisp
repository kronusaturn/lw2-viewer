(defpackage lw2-viewer.config
  (:use #:cl)
  (:export #:*site-uri* #:*graphql-uri* #:*websocket-uri*))
(in-package #:lw2-viewer.config)

(defparameter *site-uri* "http://www.example.com/") 

(defparameter *graphql-uri* "https://www.lesserwrong.com/graphql")
(defparameter *websocket-uri* "wss://www.lesserwrong.com/")
