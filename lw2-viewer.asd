(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "clss" "cl-ppcre" "xml-emitter" "city-hash" "bit-smasher" "cl-unicode")
  :components ((:file "lw2") (:file "config")))
