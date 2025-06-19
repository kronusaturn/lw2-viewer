(in-package #:lw2-viewer.config)

(reset-site-definitions)

(define-site
  :uri "http://localhost:4242/"
  :title "Example Site"
  :description "This is an example."
  :class lesswrong-viewer-site
  :main-site-title "LessWrong"
  :main-site-abbreviation "LW"
  :main-site-uri "https://www.lesswrong.com/"

; The following line will enable use of fonts.obormot.net.
; This will not work unless you have permission to use these fonts.

; :fonts-source (make-instance 'obormot-fonts-source)

  :backend ("lw2" ; Supported backends: lw2 ea-forum lw2-legacy arbital accordius
            :graphql-uri "https://www.lesswrong.com/graphql"
	    ;:magnum-crosspost-site "ea.example.com" ; set this to the hostname of another defined site to enable crosspost retrieval.

	    ; Uncomment the following for EA Forum OAuth 2.0 support.
	    ;:oauth2.0-login-uri "https://login.effectivealtruism.org/"
	    ;:oauth2.0-client-id "foo"
	    ;:oauth2.0-client-secret "bar"
	    
	    :algolia-search-uri "https://www.lesswrong.com/api/search"
            :cache-db-path "./cache/")) ; Location of the cache database. Be sure to include the trailing slash.

; (You can add more than one define-site directive.)

; Maximum size of the cache database.
; On platforms that don't support sparse files, you may want to reduce this
; to conserve disk space. Default is 2^34 or 16GB.
(defparameter *lmdb-mapsize* (expt 2 34))

; List of DNSBLs to check before allowing users to log in.
;(defparameter *dnsbl-list* (list "dnsbl.example.com"))

; List of extra resources to include on every page on every site.
;(defparameter *html-global-resources* '())

; Limit how many requests to handle in parallel, as a last ditch rate limit.
(defparameter *max-requests-in-progress* 16)
