(uiop:define-package #:lw2.dnsbl
  (:use #:cl #:lw2-viewer.config)
  (:export #:dnsbl-check))

(in-package #:lw2.dnsbl)

(defun dnsbl-check (address)
  (let ((quads (split-sequence:split-sequence #\. address)))
    (when (= (length quads) 4)
      (loop
	 for dnsbl in *dnsbl-list*
	 for result =
	   (ignore-errors
	     (usocket:get-host-by-name
	      (format nil "~{~A~^.~}.~A" (nreverse quads) dnsbl)))
	 when result return (values result dnsbl)))))
