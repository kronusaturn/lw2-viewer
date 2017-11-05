(in-package :asdf)

(asdf:defsystem :lw2-viewer
  :depends-on ("flexi-streams" "hunchentoot" "drakma" "cl-json" "lmdb" "local-time")
  :components ((:file "lw2")))
