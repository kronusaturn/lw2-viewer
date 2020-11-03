(uiop:define-package #:lw2.fonts
  (:use #:cl #:iterate #:sb-thread #:lw2.fonts-modules #:lw2.html-reader #:lw2.utils)
  (:export #:fonts-source #:google-fonts-source #:obormot-fonts-source
	   #:fonts-source-resources #:generate-fonts-html-headers)
  (:recycle #:lw2-viewer))

(in-package #:lw2.fonts)

(named-readtables:in-readtable html-reader)

;;;; google-fonts-source

(defmethod fonts-source-resources ((fonts-source google-fonts-source))
  nil)

(defmethod generate-fonts-html-headers ((fonts-source google-fonts-source))
  <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.7.2/css/all.css" integrity="sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr" crossorigin="anonymous">
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=PT+Serif:400,400i,700,700i|Assistant:400,600,800">)

;;;; obormot-fonts-source

(defparameter *obormot-fonts-stylesheet-uris*
  '("https://fonts.greaterwrong.com/?fonts=InconsolataGW,CharterGW,ConcourseGW,Whitney,MundoSans,SourceSansPro,Raleway,ProximaNova,TiredOfCourier,AnonymousPro,InputSans,InputSansNarrow,InputSansCondensed,GaramondPremierPro,TriplicateCode,TradeGothic,NewsGothicBT,Caecilia,SourceSerifPro,SourceCodePro"
    "https://fonts.greaterwrong.com/?fonts=BitmapFonts,FontAwesomeGW&base64encode=1"))
;(defparameter *obormot-fonts-stylesheet-uris* '("https://fonts.greaterwrong.com/?fonts=*"))

(defvar *fonts-redirect-data* nil)
(declaim (type (or null (unsigned-byte 63)) *fonts-redirect-last-update*))
(sb-ext:defglobal *fonts-redirect-last-update* nil)
(sb-ext:defglobal *fonts-redirect-lock* (make-mutex))
(sb-ext:defglobal *fonts-redirect-thread* nil)

(defun update-obormot-fonts ()
  (with-atomic-file-replacement (out-stream (asdf:system-relative-pathname :lw2-viewer "www/fonts.css") :element-type 'character)
    (dynamic-flet ((with-response (in-stream)
		     (let ((in-stream (ensure-character-stream in-stream)))
		       (iter (for line in-stream in-stream using #'read-line)
			     (for replaced = (ppcre:regex-replace "url\\(['\"](?!data:)" line "\\&https://fonts.greaterwrong.com/"))
			     (write-string replaced out-stream)
			     (terpri out-stream)))))
      (iter
       (for uri in *obormot-fonts-stylesheet-uris*)
       (lw2.backend:call-with-http-response
	#'with-response uri
	:headers (alist "referer" (lw2.sites::site-uri (first lw2.sites::*sites*)) "accept" "text/css,*/*;q=0.1")
	:want-stream t
	:force-binary t
	:keep-alive nil))))
  (setf *fonts-redirect-last-update* (get-unix-time)))

(defun update-obormot-fonts-async ()
  (unless *fonts-redirect-thread*
    (setf *fonts-redirect-thread*
	  (make-thread (lambda ()
			 (update-obormot-fonts)
			 (setf *fonts-redirect-thread* nil))
		       :name "obormot fonts update"))))

(defun maybe-update-obormot-fonts ()
  (let ((current-time (get-unix-time)))
    (with-mutex (*fonts-redirect-lock*)
      (let ((last-update *fonts-redirect-last-update*))
	(if last-update
	    (when (>= current-time (+ last-update (* 60 60)))
	      (update-obormot-fonts-async))
	    (update-obormot-fonts))))))

