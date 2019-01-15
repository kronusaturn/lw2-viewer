(in-package #:lw2-viewer.config)

(reset-site-definitions)

(define-site
  :uri "http://www.example.com/"
  :title "Example Site"
  :description "This is an example."
  :class lesswrong-viewer-site
  :main-site-title "LessWrong"
  :main-site-abbreviation "LW"
  :main-site-uri "https://www.lesswrong.com/"
  :backend ("lw2" ; Supported backends: lw2 lw2-legacy accordius
            :graphql-uri "https://www.lesswrong.com/graphql"
            :websocket-uri "wss://www.lesswrong.com/"
            :algolia-search-uri "https://z0gr6exqhd-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20vanilla%20JavaScript%203.24.5%3Breact-instantsearch%204.1.3%3BJS%20Helper%202.23.0&x-algolia-application-id=Z0GR6EXQHD&x-algolia-api-key=0b1d20b957917dbb5e1c2f3ad1d04ee2"
            :cache-db-path "./cache/")) ; Location of the cache database. Be sure to include the trailing slash.

; (You can add more than one define-site directive.)

; Maximum size of the cache database.
; On platforms that don't support sparse files, you may want to reduce this
; to conserve disk space. Default is 2^34 or 16GB.
(defparameter *lmdb-mapsize* (expt 2 34))
