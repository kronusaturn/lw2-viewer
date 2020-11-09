(uiop:define-package #:lw2-viewer.config
  (:use #:cl #:lw2.sites #:lw2.backend-modules #:lw2.fonts-modules)
  (:export #:*lmdb-mapsize* #:*dnsbl-list* #:*html-global-resources*)
  (:unintern #:*site-uri* #:*graphql-uri* #:*websocket-uri* #:*backend-type* #:*secure-cookies* #:*cache-db*))

(in-package #:lw2-viewer.config)

(defvar *dnsbl-list* nil)
(defvar *html-global-resources* nil)
