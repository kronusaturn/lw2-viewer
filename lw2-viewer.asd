(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("uiop" "flexi-streams" "hunchentoot" "dexador" "cl-json" "yason" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "cl-markdown" "websocket-driver-client" "ironclad" "cl-base64" "djula" "split-sequence" "cl-typesetting" "named-readtables" "collectors" "closer-mop" "chronicity" "parenscript" "trivial-gray-streams" "trivia" "iterate" "introspect-environment" "trivial-macroexpand-all" "trivial-cltl2" "dufy/core")
  :components ((:module "src"
		:components ((:file "utils" :depends-on ("macro-utils"))
			     (:file "macro-utils")
			     (:file "raw-memory-streams")
			     (:file "rwlock")
			     (:file "json")
			     (:file "graphql" :depends-on ("macro-utils" "json"))
                             (:file "hash-utils")
                             (:file "context")
			     (:file "html-reader")
			     (:file "client-script" :depends-on ("html-reader"))
			     (:file "interface-utils" :depends-on ("links" "html-reader"))
			     (:file "user-context")
			     (:file "conditions" :depends-on ("utils" "html-reader"))
			     (:file "schema-type" :depends-on ("utils" "backend-modules"))
			     (:file "dnsbl" :depends-on ("../config"))
                             (:file "backend-modules")
                             (:module "backends"
                              :components ((:file "accordius"))
                              :depends-on ("backend-modules" "backend" "lw2-login"))
			     (:file "routes")
                             (:file "sites" :depends-on ("utils" "routes" "backend-modules" "fonts-modules"))
			     (:file "resources" :depends-on ("config-package" "utils" "sites" "context"))
			     (:file "response" :depends-on ("utils" "json" "conditions" "sites" "routes" "html-reader"))
			     (:file "fonts-modules")
			     (:file "fonts" :depends-on ("html-reader" "utils" "sites" "fonts-modules" "backend" "resources"))
                             (:file "config-package" :depends-on ("sites" "backend-modules" "fonts-modules"))
                             (:module "config-copy"
				      :pathname "../"
				      :output-files (compile-op (o c) (if (file-exists-p "config.lisp") nil (list "config.lisp")))
				      :perform (compile-op :before (o c)
							   (if (file-exists-p "config.lisp")
							       (mark-operation-done o c)
							       (copy-file "config-example.lisp" "config.lisp"))))
			     (:file "../config" :depends-on ("config-copy" "config-package"))
			     (:file "lmdb" :depends-on ("rwlock" "json" "conditions" "raw-memory-streams" "hash-utils" "sites" "context" "../config"))
			     (:file "backend" :depends-on ("utils" "hash-utils" "backend-modules" "json" "lmdb" "graphql" "context" "user-context" "sites" "schema-type" "conditions" "web-push"))
			     (:file "csrf" :depends-on ("conditions" "client-script"))
                             (:file "components" :depends-on ("utils" "csrf"))
                             (:file "links" :depends-on ("utils" "lmdb" "backend" "sites" "context"))
			     (:file "legacy-archive" :depends-on ("utils" "backend"))
			     (:static-file "../text-clean-regexps.js")
			     (:static-file "../html-clean-regexps.js")
			     (:file "colors" :depends-on ("utils" "json"))
			     (:file "images" :depends-on ("conditions" "html-reader" "utils" "lmdb" "backend" "legacy-archive"))
			     (:file "elicit-predictions" :depends-on ("utils" "html-reader" "backend" "graphql"))
                             (:file "clean-html" :depends-on ("utils" "links" "lmdb" "backend" "context" "sites" "conditions" "colors" "images" "elicit-predictions" "../text-clean-regexps.js" "../html-clean-regexps.js"))
                             (:file "lw2-login" :depends-on ("utils" "json" "backend" "backend-modules" "context"))
			     (:file "backlinks" :depends-on ("html-reader" "lmdb" "backend" "backend-modules" "sites" "links" "context" "clean-html" "conditions" "utils" "interface-utils"))
			     (:file "web-push" :depends-on ("utils" "json" "conditions"))
			     (:file "push-notifications" :depends-on ("json" "backend"))
			     (:file "background-loader" :depends-on ("backend" "push-notifications" "clean-html"))
			     (:file "admin" :depends-on ("lmdb" "clean-html" "backend" "backlinks"))
			     (:module "data-viewers"
				      :components ((:file "post")
						   (:file "comment")
						   (:file "tag"))
				      :depends-on ("schema-type" "utils" "backend" "context" "user-context" "sites" "clean-html" "html-reader" "interface-utils" "links" "lmdb" "backlinks")))
                :depends-on ())
               (:module "templates"
                :components ((:static-file "conversation.html")
                             (:static-file "edit-post.html")
                             (:static-file "reset-password.html")))
               (:static-file "www/head.js")
               (:file "lw2" :depends-on ("src" "www/head.js" "templates"))
	       (:file "arbital" :depends-on ("lw2"))))
