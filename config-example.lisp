(defpackage lw2-viewer.config
  (:use #:cl)
  (:export #:*site-uri* #:*graphql-uri* #:*websocket-uri* #:*secure-cookies* #:*cache-db* #:*lmdb-mapsize*))
(in-package #:lw2-viewer.config)

(defparameter *site-uri* "http://www.example.com/") 

(defparameter *graphql-uri* "https://www.lesswrong.com/graphql")
(defparameter *websocket-uri* "wss://www.lesswrong.com/")

(defparameter *secure-cookies* nil)

; Location of the cache database. Be sure to include the trailing slash.
(defparameter *cache-db* "./cache/")

; Maximum size of the cache database.
; On platforms that don't support sparse files, you may want to reduce this
; to conserve disk space. Default is 2^34 or 16GB.
(defparameter *lmdb-mapsize* (expt 2 34))
