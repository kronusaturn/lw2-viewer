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
	    
            :algolia-search-uri "https://z0gr6exqhd-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20vanilla%20JavaScript%203.24.5%3Breact-instantsearch%204.1.3%3BJS%20Helper%202.23.0&x-algolia-application-id=Z0GR6EXQHD&x-algolia-api-key=0b1d20b957917dbb5e1c2f3ad1d04ee2"
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
