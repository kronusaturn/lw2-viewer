(uiop:define-package #:lw2.iframe-widget
  (:use #:cl #:lw2.html-reader #:lw2.backend)
  (:export #:render-iframe-widget))

(in-package #:lw2.iframe-widget)

(named-readtables:in-readtable html-reader)

;; ref: packages/lesswrong/components/lexical/embeds/IframeWidgetEmbed/IframeWidgetNode.tsx

(defun render-iframe-widget (id)
  (let ((html (get-iframe-widget-html id)))
    <iframe class="widget" sandbox="allow-scripts" srcdoc=html></iframe>))
