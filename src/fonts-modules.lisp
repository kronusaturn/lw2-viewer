(uiop:define-package #:lw2.fonts-modules
  (:use #:cl)
  (:export #:fonts-source #:google-fonts-source #:obormot-fonts-source)
  (:recycle #:lw2.fonts))

(in-package #:lw2.fonts-modules)

(defclass fonts-source () ())

(defclass google-fonts-source (fonts-source) ())

(defclass obormot-fonts-source (fonts-source) ())
