(uiop:define-package #:lw2.images
  (:use #:cl #:iterate #:split-sequence #:lw2.conditions #:lw2.html-reader #:lw2.utils #:lw2.hash-utils #:lw2.lmdb)
  (:import-from #:alexandria #:when-let #:when-let*))

(in-package #:lw2.images)

(sb-ext:defglobal *image-convert-semaphore* (sb-thread:make-semaphore :count 2))

(defun image-statistics (image-filename)
  (let* ((result-list (with-semaphore (*image-convert-semaphore*)
			(uiop:run-program (list "choom" "-n" "1000" "--" "convert" image-filename "-format" "%w %h\\n" "info:")
					  :output (lambda (stream)
						    (let ((width-height (split-sequence #\Space (read-line stream)))
							  (animation-frames 1))
						      (iter (while (read-line stream nil))
							    (incf animation-frames))
						      (list* animation-frames width-height))))))
	 (mime-type (uiop:run-program (list "file" "--brief" "--mime-type" image-filename) :output (lambda (stream) (read-line stream)))))
    (destructuring-bind (animation-frames width height) result-list
      (alist :width (parse-integer width)
	     :height (parse-integer height)
	     :animation-frames animation-frames
	     :mime-type mime-type))))

(defun string-to-brightness (color-string)
  (let ((color-value (parse-integer color-string :radix 16)))
    (cond ((= (length color-string) 8)
	   (* 3 (ldb (byte 8 24) color-value)))
	  ((= (length color-string) 6)
	   (+ (ldb (byte 8 0) color-value)
	      (ldb (byte 8 8) color-value)
	      (ldb (byte 8 16) color-value))))))

(defun image-invertible (image-filename)
  (let ((histogram-list nil)
	(background-pixels 0)
	(background-brightness 0)
	(total-pixels 0)
	(total-brightness 0.0d0))
    (with-semaphore (*image-convert-semaphore*)
      (uiop:run-program (list "choom" "-n" "1000" "--" "convert" image-filename "-format" "%c" "histogram:info:")
			:output (lambda (stream)
				  (iterate (for line next (read-line stream nil))
					   (while line)
					   (multiple-value-bind (match? strings) (ppcre:scan-to-strings "^\\s*(\\d+):" line :sharedp t)
					     (when match?
					       (let ((pixel-count (parse-integer (svref strings 0))))
						 (push pixel-count histogram-list)
						 (incf total-pixels pixel-count)
						 (multiple-value-bind (match? strings) (ppcre:scan-to-strings "#([0-9a-fA-F]+)" line :sharedp t)
						   (when match?
						     (let ((brightness (string-to-brightness (svref strings 0))))
						       (incf total-brightness (* pixel-count (/ brightness (* 3 255.0d0))))
						       (when (> pixel-count background-pixels)
							 (setf background-pixels pixel-count
							       background-brightness brightness))))))))))))
    (setf histogram-list (sort histogram-list #'>))
    (let ((tenth (first (nthcdr 10 histogram-list))))
      (and histogram-list
	   (> (/ (float background-pixels) (float total-pixels)) 0.3333333)
	   (> (/ total-brightness total-pixels) 0.5d0)
	   (> background-brightness (* 3 192))
	   (or (not tenth)
	       (> (first histogram-list) (* 10 tenth)))))))

(defun invert-image (input output)
  (with-semaphore (*image-convert-semaphore*)
    (uiop:run-program (list "choom" "-n" "1000" "--" "convert" input "-colorspace" "Lab" "-channel" "R" "-negate" "-gamma" "2.2" "-colorspace" "sRGB" output))))

(defun download-file (uri target)
  (sb-sys:with-deadline (:seconds 60)
    (with-open-file (out-stream target :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (let ((in-stream (drakma:http-request uri :want-stream t :force-binary t :connection-timeout 30 :accept "image/*,*/*")))
	(unwind-protect
	     (alexandria:copy-stream in-stream out-stream)
	  (close in-stream))))))

(define-cache-database 'lw2.backend-modules:backend-lmdb-cache "dynamic-content-images" "cached-images")

(sb-ext:defglobal *image-threads* (make-hash-table :test 'equal :synchronized t))
(defparameter *current-version* 3)

(defun process-image (uri)
  (let* ((filename (multiple-value-bind (r1 r2) (city-hash:city-hash-128 (babel:string-to-octets uri)) (format nil "~32R" (dpb r1 (byte 64 64) r2))))
	 (proxy-uri (format nil "/proxy-assets/~A" filename))
	 (pathname (format nil "www~A" proxy-uri)))
    (unless (probe-file pathname)
      (download-file uri pathname))
    (let* ((image-statistics (image-statistics pathname))
	   (inverted-uri (and (eq 1 (cdr (assoc :animation-frames image-statistics)))
			      (image-invertible pathname)
			      (format nil "~A-inverted" proxy-uri)))
	   (inverted-pathname (and inverted-uri (format nil "www~A" inverted-uri))))
      (when inverted-uri (invert-image pathname inverted-pathname))
      (alist* :version *current-version*
	      :uri uri
	      :filename filename
	      :proxy-uri proxy-uri
	      :inverted-uri inverted-uri
	      image-statistics))))

(defun image-uri-data (uri)
  (let ((key (hash-string uri)))
    (labels ((make-image-thread ()
	       (let ((thread
		      (sb-ext:with-locked-hash-table (*image-threads*)
			(or (gethash uri *image-threads*)
			    (setf (gethash uri *image-threads*)
				  (lw2.backend::make-thread-with-current-backend
				   (lambda ()
				     (log-and-ignore-errors ; FIXME figure out how to handle errors here
				      (unwind-protect
					   (let ((result (process-image uri)))
					     (cache-put "dynamic-content-images" key result :key-type :byte-vector :value-type :json)
					     (alist-bind ((filename string) (mime-type string)) result
							 (cache-put "cached-images" filename (alist :mime-type mime-type) :value-type :json))
					     result)
					(remhash uri *image-threads*))))
				   :name "image processing thread"))))))
		 (sb-thread:join-thread thread))))
      (let ((cached-data (cache-get "dynamic-content-images" key :key-type :byte-vector :value-type :json)))
	(if (and cached-data (eql (cdr (assoc :version cached-data)) *current-version*))
	    cached-data
	    (make-image-thread))))))

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

