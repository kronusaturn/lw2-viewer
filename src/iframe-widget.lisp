(uiop:define-package #:lw2.iframe-widget
  (:use #:cl #:parenscript #:lw2.html-reader #:lw2.backend)
  (:export #:render-iframe-widget))

(in-package #:lw2.iframe-widget)

(named-readtables:in-readtable html-reader)

;; ref: packages/lesswrong/components/lexical/embeds/IframeWidgetEmbed/IframeWidgetNode.tsx

(defparameter *inside-script*
  (concatenate
   'string
   "<script>"
   (ps
    (let* ((id nil))
      (labels
	  ((widget-message (message)
	     (chain window parent
		    (post-message message "*")))
	   (send-size ()
	     (request-animation-frame
	      (lambda ()
		(let* ((style (get-computed-style (chain document body)))
		       (height (+ (chain (chain document body) offset-height)
				  (or (parse-float (chain style margin-top)) 0)
				  (or (parse-float (chain style margin-bottom)) 0))))
		  (widget-message
		   (create :type "widget-size"
			   :id id
			   :height height)))))))
      (chain window
	     (add-event-listener
	      "message"
	      (lambda (event)
		(let* ((type (chain event data type)))
		  (when (string= type "widget-id")
		    (unless id
		      (setf id (chain event data id))
		      (chain (new (-resize-observer send-size))
			     (observe (chain document document-element)))
		      (send-size)))))))
      (unless id
	(widget-message (create :type "widget-request-id")))))
   "</script>")))

(defparameter *outside-script*
  (ps
   (let* ((iframe (chain document current-script previous-sibling))
	  (iframe-window (chain iframe content-window))
	  (id (chain iframe dataset widget-id)))
     (labels ((widget-message (message)
		(chain iframe-window
		       (post-message message "*"))))
       (chain window
	      (add-event-listener
	       "message"
	       (lambda (event)
		 (let ((type (chain event data type))
		       (message-id (chain event data id)))
		   (cond
		     ((string= type "widget-request-id")
		      (widget-message (create :type "widget-id"
					      :id id)))
		     ((and (string= type "widget-size")
			   (string= message-id id))
		      (request-animation-frame
		       (lambda ()
			 (setf (chain iframe style height)
			       (concatenate 'string (chain event data height) "px"))))))))))
       (widget-message
	(create :type "widget-id"
		:id id))))))

(defun render-iframe-widget (id)
  (let ((html (concatenate 'string (get-iframe-widget-html id) (inside-script id))))
    <iframe class="widget" data-widget-id=id sandbox="allow-scripts" srcdoc=html></iframe>
    <script>(with-html-stream-output (outside-script id))</script>))
