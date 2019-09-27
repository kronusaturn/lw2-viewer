(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("uiop" "flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "cl-markdown" "websocket-driver-client" "ironclad" "cl-base64" "djula" "split-sequence" "cl-typesetting" "named-readtables" "collectors" "closer-mop" "chronicity")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "graphql")
                             (:file "hash-utils")
                             (:file "context")
			     (:file "html-reader")
			     (:file "interface-utils" :depends-on ("links" "html-reader"))
			     (:file "user-context")
			     (:file "conditions" :depends-on ("html-reader"))
			     (:file "schema-type" :depends-on ("utils" "backend-modules"))
			     (:file "schema-types" :depends-on ("schema-type"))
			     (:file "dnsbl")
                             (:file "backend-modules")
                             (:module "backends"
                              :components ((:file "accordius"))
                              :depends-on ("backend-modules" "backend" "lw2-login"))
			     (:file "routes")
                             (:file "sites" :depends-on ("utils" "routes" "backend-modules" "fonts-modules"))
			     (:file "fonts-modules")
			     (:file "fonts" :depends-on ("html-reader" "utils" "sites" "fonts-modules"))
                             (:file "config-package" :depends-on ("sites" "backend-modules" "fonts-modules"))
                             (module "config-copy"
                                     :pathname "../"
                                     :output-files (compile-op (o c) (if (file-exists-p "config.lisp") nil (list "config.lisp")))
                                     :perform (compile-op :before (o c)
                                                          (if (file-exists-p "config.lisp")
                                                              (mark-operation-done o c)
                                                              (copy-file "config-example.lisp" "config.lisp"))))
                             (:file "../config" :depends-on ("config-copy" "config-package"))
                             (:file "lmdb" :depends-on ("hash-utils" "sites" "context" "../config"))
                             (:file "backend" :depends-on ("utils" "backend-modules" "lmdb" "graphql" "context" "sites" "schema-type" "schema-types" "conditions" "web-push"))
                             (:file "components" :depends-on ("utils"))
                             (:file "links" :depends-on ("lmdb" "backend" "sites" "context"))
			     (:static-file "../text-clean-regexps.js")
			     (:static-file "../html-clean-regexps.js")
                             (:file "clean-html" :depends-on ("utils" "links" "lmdb" "context" "sites" "conditions" "../text-clean-regexps.js" "../html-clean-regexps.js"))
                             (:file "lw2-login" :depends-on ("utils" "backend" "backend-modules" "context"))
			     (:file "backlinks" :depends-on ("html-reader" "lmdb" "backend" "backend-modules" "sites" "links" "context" "clean-html" "conditions"))
			     (:file "web-push" :depends-on ("utils"))
			     (:file "push-notifications" :depends-on ("backend"))
			     (:file "admin" :depends-on ("lmdb" "clean-html" "backend"))
			     (:module "data-viewers"
				      :components ((:file "post")
						   (:file "comment"))
				      :depends-on ("schema-type" "schema-types" "utils" "backend" "context" "user-context" "sites" "clean-html" "html-reader" "interface-utils" "links" "lmdb" "backlinks")))
                :depends-on ())
               (:module "templates"
                :components ((:static-file "conversation.html")
                             (:static-file "edit-post.html")
                             (:static-file "reset-password.html")))
               (:static-file "www/head.js")
               (:file "lw2" :depends-on ("src" "www/head.js" "templates"))
	       (:file "arbital" :depends-on ("lw2"))))
