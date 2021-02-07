(uiop:define-package #:lw2.images
  (:use #:cl #:iterate #:lw2.conditions #:lw2.html-reader #:lw2.utils #:lw2.hash-utils #:lw2.lmdb)
  (:import-from #:alexandria #:when-let #:when-let*))

(in-package #:lw2.images)

(sb-ext:defglobal *image-convert-semaphore* (sb-thread:make-semaphore :count 2))

(defun image-statistics (image-filename)
  (let* ((result-string (with-semaphore (*image-convert-semaphore*)
			  (uiop:run-program (list "convert" image-filename "-format" "%w %h" "info:")
					    :output :string)))
	 (result-list (split-sequence:split-sequence #\Space result-string))
	 (mime-type (uiop:run-program (list "file" "--brief" "--mime-type" image-filename) :output (lambda (stream) (read-line stream)))))
    (destructuring-bind (width height) result-list
      (values (parse-integer width) (parse-integer height) mime-type))))

(defun image-invertable (image-filename)
  (let ((histogram-list nil)
	(background-pixels 0)
	(background-brightness 0))
    (with-semaphore (*image-convert-semaphore*)
      (uiop:run-program (list "convert" image-filename "-format" "%c" "histogram:info:")
			:output (lambda (stream)
				  (iterate (for line next (read-line stream nil))
					   (while line)
					   (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^\\s*(\\d+):" line :sharedp t)
					     (when match?
					       (let ((pixel-count (parse-integer (svref strings 0))))
						 (push pixel-count histogram-list)
						 (when (> pixel-count background-pixels)
						   (multiple-value-bind (match? strings) (ppcre:scan-to-strings "#([0-9a-fA-F]+)" line :sharedp t)
						     (when match?
						       (setf background-pixels pixel-count)
						       (let* ((color-string (svref strings 0))
							      (color-value (parse-integer color-string :radix 16)))
							 (cond ((= (length color-string) 8)
								(setf background-brightness (* 3 (ldb (byte 8 0) color-value))))
							       ((= (length color-string) 6)
								(setf background-brightness (+ (ldb (byte 8 0) color-value)
											       (ldb (byte 8 8) color-value)
											       (ldb (byte 8 16) color-value))))))))))))))))
    (setf histogram-list (sort histogram-list #'>))
    (let ((tenth (first (nthcdr 10 histogram-list))))
      (and histogram-list
	   (> background-brightness (* 3 127))
	   (or (not tenth)
	       (> (first histogram-list) (* 10 tenth)))))))

(defun invert-image (input output)
  (with-semaphore (*image-convert-semaphore*)
    (uiop:run-program (list "convert" input "-colorspace" "Lab" "-channel" "R" "-negate" "-gamma" "2.2" "-colorspace" "sRGB" output))))

(defun download-file (uri target)
  (dynamic-flet ((use-response (response-stream)
		   (with-open-file (out-stream target :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
		     (alexandria:copy-stream response-stream out-stream))))
    (lw2.backend:call-with-http-response #'use-response uri :want-stream t :force-binary t :headers (alist :accept "image/*,*/*"))))

(define-cache-database 'lw2.backend-modules:backend-lmdb-cache "dynamic-content-images" "cached-images")

(sb-ext:defglobal *image-threads* (make-hash-table :test 'equal :synchronized t))

(defun process-image (uri)
  (let* ((filename (multiple-value-bind (r1 r2) (city-hash:city-hash-128 (babel:string-to-octets uri)) (format nil "~32R" (dpb r1 (byte 64 64) r2))))
	 (proxy-uri (format nil "/proxy-assets/~A" filename))
	 (pathname (format nil "www~A" proxy-uri)))
    (download-file uri pathname)
    (multiple-value-bind (width height mime-type) (image-statistics pathname)
      (let* ((inverted-uri (and (image-invertable pathname) (format nil "~A-inverted" proxy-uri)))
	     (inverted-pathname (and inverted-uri (format nil "www~A" inverted-uri))))
	(when inverted-uri (invert-image pathname inverted-pathname))
	(alist :uri uri
	       :filename filename
	       :mime-type mime-type
	       :proxy-uri proxy-uri
	       :inverted-uri inverted-uri
	       :width width
	       :height height)))))

(defun image-uri-data (uri)
  (let ((key (hash-string uri)))
    (or (cache-get "dynamic-content-images" key :key-type :byte-vector :value-type :json)
	(let ((thread
	       (sb-ext:with-locked-hash-table (*image-threads*)
		 (or (gethash uri *image-threads*)
		     (setf (gethash uri *image-threads*)
			   (lw2.backend::make-thread-with-current-backend
			    (lambda ()
			      (ignore-errors ; FIXME figure out how to handle errors here
				(unwind-protect
				     (let ((result (process-image uri)))
				       (cache-put "dynamic-content-images" key result :key-type :byte-vector :value-type :json)
				       (alist-bind ((filename string) (mime-type string)) result
						   (cache-put "cached-images" filename (alist :mime-type mime-type) :value-type :json))
				       result)
				  (remhash uri *image-threads*))))
			    :name "image processing thread"))))))
	  (sb-thread:join-thread thread)))))

(defun dynamic-image (uri container-tag-name container-attributes)
  (declare (simple-string uri container-tag-name))
  (let ((image-data
	 (log-and-ignore-errors
	  (sb-sys:with-deadline (:seconds 5)
	    (image-uri-data uri)))))
    (alist-bind ((proxy-uri (or null simple-string))
		 (inverted-uri (or null simple-string))
		 (width (or null fixnum))
		 (height (or null fixnum)))
		image-data
      (with-html-stream-output (:stream stream)
	(write-char #\< stream)
	(write-string container-tag-name stream)
	(with-delimited-writer (stream delimit :begin " " :between " ")
	  (when (and width height)
	    (delimit)
	    (format stream "style='--aspect-ratio: ~F; max-width: ~Dpx'"
		    (/ (float width)
		       (float height))
		    width))
	  (iter (for (attr . value) in container-attributes)
		(declare (type (or null simple-string) attr value))
		(unless (string-equal attr "style")
		  (delimit)
		  (write-string attr stream)
		  (write-string "='" stream)
		  (plump:encode-entities value stream)
		  (write-char #\' stream))))
	(let ((encoded-uri (plump:encode-entities uri)))
	  (if inverted-uri
	      (format stream "><picture style='display: var(--invertible-display)' data-original-src='~A'><source srcset='~A' media='(prefers-color-scheme: dark)'><img src='~A'></picture><img style='display: var(--inverted-display)' src='~A'>"
		      encoded-uri
		      inverted-uri
		      (or proxy-uri encoded-uri)
		      inverted-uri)
	      (format stream "><img src='~A' data-original-src='~A'>"
		      (or proxy-uri encoded-uri) encoded-uri)))
	(format stream "</~A>" container-tag-name)))))

