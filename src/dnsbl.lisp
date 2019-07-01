(uiop:define-package #:lw2.dnsbl
  (:use #:cl)
  (:export #:dnsbl-check))

(in-package #:lw2.dnsbl)

(defun dnsbl-check (address)
  (let ((quads (split-sequence:split-sequence #\. address)))
    (when (= (length quads) 4)
      (ignore-errors
	(usocket:get-host-by-name
	 (format nil "~{~A~^.~}.cbl.abuseat.org" (nreverse quads)))))))
