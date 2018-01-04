(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode" "parse-js" "markdown.cl" "websocket-driver-client" "ironclad" "cl-base64")
  :components ((:module "src"
			:components ((:file "lw2-login"))
			:depends-on ("config"))
	       (:file "lw2" :depends-on ("config")) (:file "config")))
