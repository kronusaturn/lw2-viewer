(uiop:define-package #:lw2.iframe-widget
  (:use #:cl #:lw2.html-reader #:lw2.backend)
  (:export #:render-iframe-widget))

(in-package #:lw2.iframe-widget)

(named-readtables:in-readtable html-reader)

(defun render-iframe-widget (id)
  (let ((html (get-iframe-widget-html id)))
    <iframe sandbox="allow-scripts" srcdoc=html></iframe>))
