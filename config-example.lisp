(defpackage lw2-viewer.config
  (:use #:cl)
  (:export #:*site-uri* #:*graphql-uri* #:*websocket-uri* #:*backend-type* #:*secure-cookies* #:*cache-db* #:*lmdb-mapsize*))
(in-package #:lw2-viewer.config)

(defparameter *site-uri* "http://www.example.com/") 

(defparameter *graphql-uri* "https://www.lesswrong.com/graphql")
(defparameter *websocket-uri* "wss://www.lesswrong.com/")

; Supported backends: lw2 accordius
(defparameter *backend-type* "lw2")

(defparameter *secure-cookies* nil)

; Location of the cache database. Be sure to include the trailing slash.
(defparameter *cache-db* "./cache/")

; Maximum size of the cache database.
; On platforms that don't support sparse files, you may want to reduce this
; to conserve disk space. Default is 2^34 or 16GB.
(defparameter *lmdb-mapsize* (expt 2 34))
