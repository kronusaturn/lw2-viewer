(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("uiop" "flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "markdown.cl" "websocket-driver-client" "ironclad" "cl-base64" "djula" "split-sequence")
  :components ((:module "src"
                :components ((:file "lmdb")
                             (:file "backend" :depends-on ("lmdb"))
                             (:file "links" :depends-on ("lmdb" "backend"))
                             (:file "clean-html" :depends-on ("links" "lmdb"))
                             (:file "lw2-login"))
                :depends-on ("config"))
               (:static-file "www/head.js")
               (:static-file "templates/edit-post.html")
               (:static-file "templates/reset-password.html")
               (module "config-copy"
                       :pathname ""
                       :output-files (compile-op (o c) (list "config.lisp"))
                       :components
                       ((:static-file "config-example.lisp"))
                       :perform (compile-op :before (o c)
                                            (if (not (uiop:file-exists-p "config.lisp"))
                                                (uiop:copy-file "config-example.lisp" "config.lisp"))))
               (:file "lw2" :depends-on ("src" "config" "www/head.js" "templates/edit-post.html" "templates/reset-password.html"))
               (:file "config" :depends-on ("config-copy"))))
