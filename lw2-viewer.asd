(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("uiop" "flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "markdown.cl" "websocket-driver-client" "ironclad" "cl-base64" "djula" "split-sequence" "cl-typesetting")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "graphql")
                             (:file "hash-utils")
                             (:file "lmdb" :depends-on ("hash-utils"))
                             (:file "backend" :depends-on ("utils" "lmdb" "graphql"))
                             (:file "links" :depends-on ("lmdb" "backend"))
			     (:static-file "../text-clean-regexps.js")
			     (:static-file "../html-clean-regexps.js")
                             (:file "clean-html" :depends-on ("links" "lmdb" "../text-clean-regexps.js" "../html-clean-regexps.js"))
                             (:file "lw2-login" :depends-on ("utils" "backend")))
                :depends-on ("config"))
               (:module "templates"
                :components ((:static-file "conversation.html")
                             (:static-file "edit-post.html")
                             (:static-file "reset-password.html")))
               (:static-file "www/head.js")
               (module "config-copy"
                       :pathname ""
                       :output-files (compile-op (o c) (if (file-exists-p "config.lisp") nil (list "config.lisp")))
                       :perform (compile-op :before (o c)
                                            (if (file-exists-p "config.lisp")
                                                (mark-operation-done o c)
                                                (copy-file "config-example.lisp" "config.lisp"))))
               (:file "lw2" :depends-on ("src" "config" "www/head.js" "templates"))
               (:file "config" :depends-on ("config-copy"))))
