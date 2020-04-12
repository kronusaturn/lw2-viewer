(uiop:define-package #:lw2.fonts
  (:use #:cl #:sb-thread #:lw2.fonts-modules #:lw2.html-reader #:lw2.utils)
  (:export #:fonts-source #:google-fonts-source #:obormot-fonts-source
	   #:generate-fonts-html-headers)
  (:recycle #:lw2-viewer))

(in-package #:lw2.fonts)

(named-readtables:in-readtable html-reader)

;;;; google-fonts-source

(defmethod generate-fonts-html-headers ((fonts-source google-fonts-source))
  <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.7.2/css/all.css" integrity="sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr" crossorigin="anonymous">
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=PT+Serif:400,400i,700,700i|Assistant:400,600,800">)

;;;; obormot-fonts-source

(defparameter *obormot-fonts-stylesheet-uris*
  '("https://fonts.greaterwrong.com/?fonts=InconsolataGW,CharterGW,ConcourseGW,Whitney,MundoSans,SourceSansPro,Raleway,ProximaNova,TiredOfCourier,AnonymousPro,InputSans,InputSansNarrow,InputSansCondensed,GaramondPremierPro,TriplicateCode,TradeGothic,NewsGothicBT,Caecilia,SourceSerifPro,SourceCodePro"
    "https://fonts.greaterwrong.com/?fonts=BitmapFonts,FontAwesomeGW&base64encode=1"))
;(defparameter *obormot-fonts-stylesheet-uris* '("https://fonts.greaterwrong.com/?fonts=*"))

(defvar *fonts-redirect-data* nil)
(sb-ext:defglobal *fonts-redirect-lock* (make-mutex))
(sb-ext:defglobal *fonts-redirect-thread* nil)

(defun generate-fonts-links ()
  (let ((current-time (get-unix-time)))
    (labels ((get-redirects (uri-list)
               (loop for request-uri in uri-list collect
                     (multiple-value-bind (body status headers uri)
                       (dex:request request-uri :method :head :max-redirects 0 :headers (alist "referer" (lw2.sites::site-uri (first lw2.sites::*sites*)) "accept" "text/css,*/*;q=0.1"))
                       (declare (ignore body uri))
                       (let ((location (gethash "location" headers)))
                         (if (and (typep status 'integer) (< 300 status 400) location)
                             location
                             nil)))))
             (update-redirects ()
               (handler-case
                 (let* ((new-redirects (get-redirects *obormot-fonts-stylesheet-uris*))
                        (new-redirects (loop for new-redirect in new-redirects
                                             for original-uri in *obormot-fonts-stylesheet-uris*
                                             collect (if new-redirect (quri:render-uri (quri:merge-uris (quri:uri new-redirect) (quri:uri original-uri))) original-uri))))
                   (with-mutex (*fonts-redirect-lock*) (setf *fonts-redirect-data* (list *obormot-fonts-stylesheet-uris* new-redirects current-time)
                                                             *fonts-redirect-thread* nil))
                   new-redirects)
                 (serious-condition () *obormot-fonts-stylesheet-uris*)))
             (ensure-update-thread ()
               (with-mutex (*fonts-redirect-lock*)
                 (or *fonts-redirect-thread*
                     (setf *fonts-redirect-thread* (make-thread #'update-redirects :name "obormot fonts redirect update"))))))
      (destructuring-bind (&optional base-uris redirect-uris timestamp) (with-mutex (*fonts-redirect-lock*) *fonts-redirect-data*)
        (if (and (eq base-uris *obormot-fonts-stylesheet-uris*) timestamp)
          (progn
            (if (>= current-time (+ timestamp 60))
                (ensure-update-thread))
            (or redirect-uris *obormot-fonts-stylesheet-uris*))
          (update-redirects))))))

(defmethod generate-fonts-html-headers ((fonts-source obormot-fonts-source))
  (dolist (stylesheet (generate-fonts-links))
    <link rel="stylesheet" href=stylesheet>))
