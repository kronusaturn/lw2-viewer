(in-package #:lw2-viewer.config)

(reset-site-definitions)

(define-site
  :uri "http://www.example.com/"
  :title "Example Site"
  :class lesswrong-viewer-site
  :backend ("lw2" ; Supported backends: lw2 lw2-legacy accordius
            :graphql-uri "https://www.lesswrong.com/graphql"
            :websocket-uri "wss://www.lesswrong.com/"
            :cache-db-path "./cache/")) ; Location of the cache database. Be sure to include the trailing slash.

; (You can add more than one define-site directive.)

; Maximum size of the cache database.
; On platforms that don't support sparse files, you may want to reduce this
; to conserve disk space. Default is 2^34 or 16GB.
(defparameter *lmdb-mapsize* (expt 2 34))
