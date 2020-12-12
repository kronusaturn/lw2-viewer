(uiop:define-package #:lw2.resources
  (:use #:cl #:lw2-viewer.config #:lw2.sites #:lw2.context)
  (:export #:*page-resources* #:with-page-resources #:require-resource #:generate-versioned-link #:fonts-source-resources #:site-resources)
  (:recycle #:lw2-viewer))

(in-package #:lw2.resources)

(defparameter *page-resources* nil)

(defmacro with-page-resources (&body body)
  `(let ((*page-resources* (site-resources *current-site*)))
     ,@body))

(defun require-resource (type &rest args)
  (push (list* type args) *page-resources*))

(defun generate-versioned-link (file)
  (let* ((filename (format nil "www~A" file))
	 (stat (sb-posix:stat filename)))
    (format nil "~A?v=~A" file (sb-posix:stat-mtime stat))))

(defgeneric fonts-source-resources (site))

(defgeneric site-resources (site)
  (:method-combination append :most-specific-first)
  (:method append ((s site))
    (labels ((gen-inner (theme os &optional dark-preference)
	       (list :stylesheet (generate-versioned-link (format nil "/css/style~@[-~A~].~A.css" theme os))
		     :media (if dark-preference "(prefers-color-scheme: dark)")
		     :class "theme"))
	     (gen-theme (theme os)
	       (if theme
		   (list (gen-inner theme os))
		   (list (gen-inner "dark" os t)
			 (gen-inner nil os)))))
      (let* ((ua (hunchentoot:header-in* :user-agent))
	     (theme (hunchentoot:cookie-in "theme"))
	     (theme (if (and theme (> (length theme) 0)) theme))
	     (os (cond ((search "Windows" ua) "windows")
		       ((search "Mac OS" ua) "mac")
		       (t "linux"))))
	(append
	 *html-global-resources*
	 (fonts-source-resources (site-fonts-source s))
	 (list*
	  (list :script (generate-versioned-link "/head.js"))
	  (list :async-script (generate-versioned-link "/script.js"))
	  (handler-case (gen-theme theme os)
	    (serious-condition () (gen-theme nil os)))))))))

(hunchentoot:define-easy-handler (view-versioned-resource :uri (lambda (r)
                                                                 (multiple-value-bind (file content-type)
                                                                   #.(labels ((defres (uri content-type)
                                                                                `(,uri (values (concatenate 'string "www" ,uri) ,content-type))))
                                                                       (concatenate 'list
                                                                                    '(alexandria:switch ((hunchentoot:script-name r) :test #'string=))
                                                                                    (loop for system in '("mac" "windows" "linux") nconc
                                                                                      (loop for theme in '(nil "dark" "grey" "ultramodern" "zero" "brutalist" "rts" "classic" "less")
                                                                                            collect (defres (format nil "/css/style~@[-~A~].~A.css" theme system) "text/css")))
                                                                                    (loop for (uri content-type) in
										      '(("/fonts.css" "text/css")
											("/arbital.css" "text/css")
											("/head.js" "text/javascript")
											("/script.js" "text/javascript")
											("/assets/favicon.ico" "image/x-icon")
											("/assets/telegraph.jpg" "image/jpeg")
											("/assets/popup.svg" "image/svg+xml"))
                                                                                      collect (defres uri content-type))))
                                                                   (when file
                                                                     (when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
                                                                       (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
                                                                     (hunchentoot:handle-static-file file content-type)
                                                                     t))))
    nil)
