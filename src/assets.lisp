(uiop:define-package #:lw2.assets
  (:use #:cl)
  (:export #:generate-versioned-link #:get-asset-versions))

(in-package #:lw2.assets)

(defparameter *assets*
  (append (loop for system in '("mac" "windows" "linux") nconc
	       (loop for theme in '(nil "dark" "grey" "ultramodern" "zero" "brutalist" "rts" "classic" "less")
		  for uri = (format nil "/css/style~@[-~A~].~A.css" theme system)
		  for filename = (format nil "www~A" uri)
		  collect (list uri filename "text/css")))
	  (loop for (uri content-type) in
	       '(("/script.js" "text/javascript")
	         ("/luser.js" "text/javascript")
	         ("/image-focus.js" "text/javascript")
	         ("/assets/favicon.ico" "image/x-icon")
	         ("/css/theme_tweaker.css" "text/css")
	         ("/assets/icons.svg" "image/svg+xml"))
	     for filename = (format nil "www~A" uri)
	     collect (list uri filename content-type))))

(defun generate-versioned-link (file)
  (format nil "~A?v=~A" file (sb-posix:stat-mtime (sb-posix:stat (format nil "www~A" file))))) 

(defun get-asset-versions ()
  ;; TODO: only stat files that are actually needed
  (loop for (uri filename content-type) in *assets*
       collect (cons uri (generate-versioned-link uri))))

(hunchentoot:define-easy-handler
    (view-versioned-resource :uri (lambda (r)
				    (multiple-value-bind (file content-type)
					(loop for (uri file content-type) in *assets*
					   when (string= uri (hunchentoot:script-name r))
					   return (values file content-type))
				      (when file
					(when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
					  (setf (hunchentoot:header-out "Cache-Control") (format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
					(hunchentoot:handle-static-file file content-type)
					t))))
    nil)
