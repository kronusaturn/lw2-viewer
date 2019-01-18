(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("uiop" "flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "markdown.cl" "websocket-driver-client" "ironclad" "cl-base64" "djula" "split-sequence" "cl-typesetting" "named-readtables")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "graphql")
                             (:file "hash-utils")
                             (:file "context")
			     (:file "html-reader")
                             (:file "backend-modules")
                             (:module "backends"
                              :components ((:file "accordius"))
                              :depends-on ("backend-modules" "backend" "lw2-login"))
                             (:file "sites" :depends-on ("utils" "backend-modules"))
                             (:file "config-package" :depends-on ("sites" "backend-modules"))
                             (module "config-copy"
                                     :pathname "../"
                                     :output-files (compile-op (o c) (if (file-exists-p "config.lisp") nil (list "config.lisp")))
                                     :perform (compile-op :before (o c)
                                                          (if (file-exists-p "config.lisp")
                                                              (mark-operation-done o c)
                                                              (copy-file "config-example.lisp" "config.lisp"))))
                             (:file "../config" :depends-on ("config-copy" "config-package"))
                             (:file "lmdb" :depends-on ("hash-utils" "sites" "context" "../config"))
                             (:file "backend" :depends-on ("utils" "backend-modules" "lmdb" "graphql" "context" "sites"))
                             (:file "components" :depends-on ("utils"))
                             (:file "links" :depends-on ("lmdb" "backend" "sites" "context"))
			     (:static-file "../text-clean-regexps.js")
			     (:static-file "../html-clean-regexps.js")
                             (:file "clean-html" :depends-on ("utils" "links" "lmdb" "context" "sites" "../text-clean-regexps.js" "../html-clean-regexps.js"))
                             (:file "lw2-login" :depends-on ("utils" "backend" "backend-modules" "context")))
                :depends-on ())
               (:module "templates"
                :components ((:static-file "conversation.html")
                             (:static-file "edit-post.html")
                             (:static-file "reset-password.html")))
               (:static-file "www/head.js")
               (:file "lw2" :depends-on ("src" "www/head.js" "templates"))))
