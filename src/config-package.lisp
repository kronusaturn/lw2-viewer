(uiop:define-package lw2-viewer.config
  (:use #:cl #:lw2.sites #:lw2.backend-modules)
  (:export #:*lmdb-mapsize*)
  (:unintern #:*site-uri* #:*graphql-uri* #:*websocket-uri* #:*backend-type* #:*secure-cookies* #:*cache-db*))

(in-package #:lw2-viewer.config)
