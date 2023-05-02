(uiop:define-package #:lw2.resources
  (:use #:cl #:iterate #:lw2-viewer.config #:lw2.utils #:lw2.sites #:lw2.context)
  (:import-from #:alexandria #:with-gensyms #:when-let #:appendf)
  (:export #:*page-resources* #:inverted-media-query #:with-page-resources #:require-resource #:generate-versioned-link #:with-resource-bindings #:call-with-fonts-source-resources #:site-resources)
  (:recycle #:lw2-viewer)
  (:unintern #:fonts-source-resources))

(in-package #:lw2.resources)

(defparameter *page-resources* nil)
(defparameter *link-header* (constantly nil))
(defparameter *style-tags* (constantly nil))
(defparameter *script-tags* (constantly nil))
(defparameter *async-script-tags* (constantly nil))

(defparameter *push-option* nil)

(defun inverted-media-query ()
  (alexandria:switch ((hunchentoot:cookie-in "dark-mode") :test #'string-equal)
		     ("dark" "all")
		     ("light" "not all")
		     (t (if (string= (hunchentoot:cookie-in "theme") "dark")
			    "all"
			    "all and (prefers-color-scheme: dark)"))))

(defmacro with-page-resources (&body body)
  `(let* ((*link-header* *link-header*)
	  (*style-tags* *style-tags*)
	  (*script-tags* *script-tags*)
	  (*async-script-tags* *async-script-tags*)
	  (*push-option* (when (hunchentoot:cookie-in "push") "nopush"))
	  (*page-resources* (site-resources *current-site*)))
     (dynamic-flet ((fn () ,@body))
       (call-with-site-resources *current-site* #'fn))))

(defmacro with-appended-functions ((&rest clauses) &body body)
  `(let* ,(iter (for (old-function new-name lambda-list . inner-body) in clauses)
		(let ((old-function-name (gensym)))
		  (collect `(,old-function-name ,old-function))
		  (collect `(,new-name (lambda ,lambda-list (funcall ,old-function-name ,@lambda-list) ,@inner-body)))))
     ,@body))

(defun require-resource (type &rest args)
  (push (list* type args) *page-resources*))

(defun generate-versioned-link (file)
  (let* ((filename (format nil "www~A" file)))
    (or (ignore-errors (format nil "~A?v=~A" file (universal-time-to-unix (file-write-date filename))))
	file)))

(defgeneric call-with-fonts-source-resources (site fn))

(defun output-link-header-element (stream uri type)
  (multiple-value-bind (rel type as push-option)
      (case type
	(:preconnect (values "preconnect"))
	(:stylesheet (values "preload" "text/css" "style" *push-option*))
	(:script (values "preload" "text/javascript" "script" *push-option*)))
    (format stream "<~A>;rel=~A~@[;type=~A~]~@[;as=~A~]~@[;~A~]" uri rel type as push-option)))

(defun output-style-tag (stream uri media class)
  (format stream "<link rel=\"stylesheet\" href=\"~A\"~@[ media=\"~A\"~]~@[ class=\"~A\"~]>" uri media class))

(defun output-script-tag (stream uri &key async)
  (format stream "<script src=\"~A\"~:[~; async~]></script>" uri async))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun preconnect-resource-forms (stream uri)
    (alist :link-header `((funcall delimit) (output-link-header-element ,stream ,uri :preconnect))))

  (defun style-resource-forms (stream uri &optional media class)
    (with-gensyms (versioned-uri)
      (alist :bindings `((,versioned-uri (generate-versioned-link ,uri)))
	     :link-header `((funcall delimit) (output-link-header-element ,stream ,versioned-uri :stylesheet))
	     :style-tags `((output-style-tag ,stream ,versioned-uri ,media ,class)))))

  (defun script-resource-forms (stream uri &rest args &key async)
    (with-gensyms (versioned-uri)
      (alist :bindings `((,versioned-uri (generate-versioned-link ,uri)))
	     :link-header `((funcall delimit) (output-link-header-element ,stream ,versioned-uri :script))
	     (if async :async-script-tags :script-tags) `((output-script-tag ,stream ,versioned-uri ,@args))))))

(defmacro with-resource-bindings ((&rest clauses) &body body)
  (let ((forms (make-hash-table :test 'eq)))
    (iter (for (resource-type . params) in clauses)
	  (iter (for (form-type . form-body) in (apply (case resource-type
							 (:preconnect #'preconnect-resource-forms)
							 (:style #'style-resource-forms)
							 (:script #'script-resource-forms))
						       'stream params))
		(appendf (gethash form-type forms) form-body)))
    `(let* ,(gethash :bindings forms)
       ,(let (w-a-f-clauses special-names lexical-names)
	     (iter (for (form-type special-name lexical-name extra-lambda-list)
			in '((:link-header *link-header* link-header-fn (delimit))
			     (:style-tags *style-tags* style-tags-fn nil)
			     (:script-tags *script-tags* script-tags-fn nil)
			     (:async-script-tags *async-script-tags* async-script-tags-fn nil)))
		   (when-let ((form-body (gethash form-type forms)))
			     (push `(,special-name ,lexical-name (stream ,@extra-lambda-list) ,@form-body) w-a-f-clauses)
			     (push special-name special-names)
			     (push lexical-name lexical-names)))
	     `(with-appended-functions ,(reverse w-a-f-clauses)
		,(when body `(declare (dynamic-extent ,@lexical-names)))
		(setf ,@(iter (for special in special-names) (for lexical in lexical-names)
			      (collect special) (collect lexical)))
		,@body)))))

(defgeneric call-with-site-resources (site fn)
  (:method ((site site) fn)
    (with-resource-bindings ((:script "/head.js")
			     (:script "/script.js" :async t))
      (call-with-fonts-source-resources (site-fonts-source site) fn))))

(defun file-valid-date (file)
  (max (file-write-date file)
       (load-time-value (get-universal-time))))

(defgeneric site-resources (site)
  (:method-combination append :most-specific-first)
  (:method append ((s site))
    (labels ((gen-theme (theme os)
	       (let* ((basename (format nil "~@[-~A~].~A.css" theme os))
		      (filename (format nil "www/css/style~A" basename))
		      (version (universal-time-to-unix (file-valid-date filename)))
		      (baseurl (format nil "~A?v=~A" basename version)))
		 (with-resource-bindings ((:style (format nil "/generated-css/style~A" baseurl) nil "theme")
					  (:style (format nil "/generated-css/colors~A" baseurl) nil "theme light-mode")
					  (:style (format nil "/generated-css/inverted~A" baseurl) (inverted-media-query) "theme dark-mode"))))))
      (let* ((ua (hunchentoot:header-in* :user-agent))
	     (theme (or (and *preview* (nonempty-string (hunchentoot:get-parameter "theme")))
			(nonempty-string (hunchentoot:cookie-in "theme"))))
	     (theme (if (or (string= theme "default") (string= theme "dark")) nil theme))
	     (os (cond ((search "Windows" ua) "windows")
		       ((search "Mac OS" ua) "mac")
		       (t "linux"))))
	(handler-case (gen-theme theme os)
	  (serious-condition () (gen-theme nil os)))
	*html-global-resources*))))

(sb-ext:defglobal *static-assets* nil)

(let ((new-static-assets (make-hash-table :test 'equal)))
  (flet ((defres (uri content-type)
	   (vector (concatenate 'string "www" uri) content-type)))
    (loop for system in '("mac" "windows" "linux") nconc
	 (loop for theme in '(nil "dark" "grey" "ultramodern" "zero" "brutalist" "rts" "classic" "less")
	    do (let ((uri (format nil "/css/style~@[-~A~].~A.css" theme system)))
		 (setf (gethash uri new-static-assets) (defres uri "text/css")))))
    (loop for (uri content-type) in
	 '(("/fonts.css" "text/css")
	   ("/arbital.css" "text/css")
	   ("/head.js" "text/javascript")
	   ("/script.js" "text/javascript")
	   ("/assets/favicon.ico" "image/x-icon")
	   ("/assets/telegraph.jpg" "image/jpeg")
	   ("/assets/popup.svg" "image/svg+xml"))
       do (setf (gethash uri new-static-assets) (defres uri content-type))))
  (setf *static-assets* new-static-assets))

(hunchentoot:define-easy-handler
    (view-versioned-resource
     :uri (lambda (r)
	    (when-let ((asset-data (gethash (hunchentoot:script-name r) *static-assets*)))
	      (let ((file (svref asset-data 0))
		    (content-type (svref asset-data 1)))
		(when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
		  (setf (hunchentoot:header-out "Cache-Control") #.(format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
		(hunchentoot:handle-static-file file content-type))
	      t)))
    nil)

(defun process-css-line (in-line)
  (macrolet ((override (name)
	       `(let ((reg-matches (nth-value 1 (ppcre:scan-to-strings ,(format nil "~A:\\s*(.*?)\\s*(?:;|\\*/|$)" name) in-line))))
		  (if (>= (length reg-matches) 1) (aref reg-matches 0)))))
    (let* ((invert-override (override "invert-override"))
	   (gamma-override (override "gamma-override"))
	   (backgroundp (to-boolean (ppcre:scan "background|shadow" in-line)))
	   (gamma (cond (gamma-override (parse-float:parse-float gamma-override :type 'double-float))
			(backgroundp 1.6d0)
			(t 2.2d0))))
      (values
       (format nil "--theme~A-color"
	       (cond (invert-override (format nil "-override-~A" (multiple-value-call #'lw2.colors::safe-color-name (lw2.colors::decode-css-color invert-override))))
		     (gamma-override (format nil "-gamma-~F" gamma))
		     (backgroundp "-bg")
		     (t "")))
       invert-override
       gamma))))

(defun output-tweakable-css (file out-stream)
  (with-open-file (in-stream file)
    (loop for in-line = (read-line in-stream nil)
       while in-line
       do (let* ((variable-prefix (process-css-line in-line))
		 (out-line (ppcre:regex-replace-all
			    lw2.colors::-css-color-scanner-
			    in-line
			    (lambda (target-string start end match-start match-end reg-starts reg-ends)
			      (declare (ignore start end reg-starts reg-ends))
			      (let ((color-string (substring target-string match-start match-end)))
				(format nil "var(~A-~A)"
					variable-prefix
					(multiple-value-call #'lw2.colors::safe-color-name (lw2.colors::decode-css-color color-string))))))))
	    (write-line out-line out-stream)))))

(defun output-css-colors (file out-stream invert)
  (let ((used-colors (make-hash-table :test 'equal)))
    (with-open-file (in-stream file)
      (format out-stream ":root {~%")
      (loop for in-line = (read-line in-stream nil)
	 while in-line
	 do (multiple-value-bind (variable-prefix invert-override gamma)
		(process-css-line in-line)
	      (ppcre:do-matches-as-strings (color-string lw2.colors::-css-color-scanner- in-line)
		(multiple-value-bind (r g b a) (lw2.colors::decode-css-color color-string)
		  (let ((color-name (format nil "~A-~A"
					    variable-prefix
					    (lw2.colors::safe-color-name r g b a))))
		    (unless (gethash color-name used-colors)
		      (setf (gethash color-name used-colors) t)
		      (format out-stream "~A: ~A;~%"
			      color-name
			      (if invert
				  (or invert-override
				      (multiple-value-call #'lw2.colors::encode-css-color (lw2.colors::perceptual-invert-rgba r g b a gamma)))
				  color-string))))))))))
  (format out-stream "}~%"))

(sb-ext:defglobal *css-generator-lock* (sb-thread:make-mutex :name "CSS generator lock"))

(hunchentoot:define-easy-handler
    (view-generated-css
     :uri (lambda (r)
	    (let ((regs (nth-value 1 (ppcre:scan-to-strings "^/generated-css/(style|colors|inverted)([-.][a-z.-]+)$" (hunchentoot:script-name r)))))
	      (when (= (length regs) 2)
		(setf (hunchentoot:header-out "Content-Type") "text/css")
		(when (assoc "v" (hunchentoot:get-parameters r) :test #'string=)
		  (setf (hunchentoot:header-out "Cache-Control") #.(format nil "public, max-age=~A, immutable" (- (expt 2 31) 1))))
		(let ((source-file (format nil "www/css/style~A" (aref regs 1)))
		      (cache-file (format nil "www/generated-css/~A~A" (aref regs 0) (aref regs 1)))
		      (out-stream (flex:make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
		  (flet ((cache-valid-p () (and (probe-file cache-file) (<= (file-valid-date source-file) (file-write-date cache-file)))))
		    (unless (cache-valid-p)
		      (sb-thread:with-mutex (*css-generator-lock*)
			(unless (cache-valid-p)
			  (ensure-directories-exist cache-file)
			  (with-atomic-file-replacement (cache-stream cache-file :element-type 'character :external-format :utf-8)
			    (cond
			      ((string= (aref regs 0) "style")
			       (output-tweakable-css source-file cache-stream))
			      ((string= (aref regs 0) "colors")
			       (output-css-colors source-file cache-stream nil))
			      ((string= (aref regs 0) "inverted")
			       (output-css-colors source-file cache-stream t))))))))
		  (with-open-file (cache-stream cache-file :direction :input :element-type '(unsigned-byte 8))
		    (alexandria:copy-stream cache-stream out-stream)))
		t))))
    nil)
