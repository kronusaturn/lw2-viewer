(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time" "plump" "cl-ppcre" "xml-emitter")
  :components ((:file "lw2") (:file "config")))
