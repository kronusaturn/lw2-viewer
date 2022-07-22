(uiop:define-package #:lw2.images
  (:use #:cl #:iterate #:split-sequence #:lw2.conditions #:lw2.html-reader #:lw2.utils #:lw2.hash-utils #:lw2.lmdb)
  (:import-from #:alexandria #:when-let #:when-let*))

(in-package #:lw2.images)

(defparameter *wrapper-program* #+linux '("choom" "-n" "1000" "--") #-linux nil)

(sb-ext:defglobal *image-convert-semaphore* (sb-thread:make-semaphore :count 2))

(defmacro run-program (program-args &rest lisp-args)
  `(uiop:run-program (append *wrapper-program* (list ,@program-args)) ,@lisp-args))

(defun image-statistics (image-filename)
  (let* ((result-list (with-semaphore (*image-convert-semaphore*)
			(run-program ("convert" image-filename "-format" "%w %h\\n" "info:")
				     :output (lambda (stream)
					       (let ((width-height (split-sequence #\Space (read-line stream)))
						     (animation-frames 1))
						 (iter (while (read-line stream nil))
						       (incf animation-frames))
						 (list* animation-frames width-height))))))
	 (mime-type (run-program ("file" "--brief" "--mime-type" image-filename) :output (lambda (stream) (read-line stream)))))
    (destructuring-bind (animation-frames width height) result-list
      (alist :width (parse-integer width)
	     :height (parse-integer height)
	     :animation-frames animation-frames
	     :mime-type mime-type))))

(defun string-to-brightness (color-string)
  (let ((color-value (parse-integer color-string :radix 16)))
    (cond ((= (length color-string) 8) ; 8-bit rgba
	   (* 3 (ldb (byte 8 24) color-value)))
	  ((= (length color-string) 6) ; 8-bit rgb
	   (+ (ldb (byte 8 0) color-value)
	      (ldb (byte 8 8) color-value)
	      (ldb (byte 8 16) color-value)))
	  ((= (length color-string) 16) ; 16-bit rgba
	   (floor
	    (* 3 (ldb (byte 16 48) color-value))
	    256))
	  ((= (length color-string) 12) ; 16-bit rgb
	   (floor
	    (+ (ldb (byte 16 0) color-value)
	       (ldb (byte 16 16) color-value)
	       (ldb (byte 16 32) color-value))
	    256)))))

(defun image-invertible (image-filename)
  (let ((histogram-list nil)
	(background-pixels 0)
	(background-brightness 0)
	(total-pixels 0)
	(total-brightness 0.0d0))
    (with-semaphore (*image-convert-semaphore*)
      (run-program ("convert" image-filename "-format" "%c" "histogram:info:")
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
    (run-program ("convert" input "-colorspace" "Lab" "-channel" "R" "-negate" "-gamma" "2.2" "-colorspace" "sRGB" output))))

(defun download-file (uri target)
  (sb-sys:with-deadline (:seconds 60)
    (with-open-file (out-stream target :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (with-open-stream (in-stream (dex:get uri :want-stream t :force-binary t :keep-alive nil :connect-timeout 30 :headers '((:accept . "image/*,*/*"))))
	(alexandria:copy-stream in-stream out-stream)))))

(defun download-file-with-wayback-fallback (uri target)
  (handler-case
      (download-file uri target)
    (error (c)
      (let ((wayback-uri (lw2.legacy-archive:wayback-unmodified-url uri)))
	(if wayback-uri
	    (download-file wayback-uri target)
	    (error c))))))

(define-cache-database 'lw2.backend-modules:backend-lmdb-cache "dynamic-content-images" "cached-images")

(sb-ext:defglobal *image-threads* (make-hash-table :test 'equal :synchronized t))
(defparameter *current-version* 3)

(defun filename-to-uri (filename)
  (concatenate 'base-string "/proxy-assets/" filename))

(defun uri-to-pathname (uri)
  (concatenate 'base-string "www" uri))

(defun process-image (uri)
  (let* ((filename (multiple-value-bind (r1 r2) (city-hash:city-hash-128 (babel:string-to-octets uri)) (format nil "~32R" (dpb r1 (byte 64 64) r2))))
	 (proxy-uri (filename-to-uri filename))
	 (pathname (uri-to-pathname proxy-uri)))
    (download-file-with-wayback-fallback uri pathname)
    (let* ((image-statistics (image-statistics pathname))
	   (inverted-uri (and (eq 1 (cdr (assoc :animation-frames image-statistics)))
			      (image-invertible pathname)
			      (concatenate 'base-string proxy-uri "-inverted")))
	   (inverted-pathname (and inverted-uri (uri-to-pathname inverted-uri))))
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
	(alist-bind ((proxy-uri (or null simple-string))
		     (inverted-uri (or null simple-string))
		     (version))
		    cached-data
	  (if (and cached-data (eql version *current-version*)
		   proxy-uri (probe-file (uri-to-pathname proxy-uri))
		   (or (not inverted-uri) (probe-file (uri-to-pathname inverted-uri))))
	      cached-data
	      (make-image-thread)))))))

(defun dynamic-image (uri container-tag-name container-attributes img-attributes)
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
      (labels ((write-attributes (attrs predicate stream)
		 (iter (for (attr . value) in attrs)
		       (declare (type (or null simple-string) attr value))
		       (when (and attr (funcall predicate attr))
			 (write-char #\Space stream)
			 (write-string attr stream)
			 (when value
			   (write-string "='" stream)
			   (plump:encode-entities value stream)
			   (write-char #\' stream)))))
	       (finish-tag (attrs predicate stream)
		 (write-attributes attrs predicate stream)
		 (write-char #\> stream)))
      (with-html-stream-output (:stream stream)
	(write-char #\< stream)
	(write-string container-tag-name stream)
	(when (and width height)
	  (write-char #\Space stream)
	  (format stream "style='--aspect-ratio: ~F; max-width: ~Dpx'"
		  (/ (float width)
		     (float height))
		  width))
	(finish-tag container-attributes (lambda (attr) (not (string-equal attr "style"))) stream)
	(let ((encoded-uri (plump:encode-entities uri))
	      (predicate (lambda (attr) (string-equal attr "alt"))))
	  (cond
	    (inverted-uri
	     (format stream "<picture style='display: var(--invertible-display)' data-original-src='~A'><source srcset='~A' media='(prefers-color-scheme: dark)'><img src='~A' loading='lazy'"
		      encoded-uri
		      inverted-uri
		      (or proxy-uri encoded-uri))
	     (finish-tag img-attributes predicate stream)
	     (format stream "</picture><img style='display: var(--inverted-display)' loading='lazy' src='~A'"
		     inverted-uri)
	     (finish-tag img-attributes predicate stream))
	    (:otherwise
	      (format stream "<img src='~A' data-original-src='~A' loading='lazy'"
		      (or proxy-uri encoded-uri) encoded-uri)
	      (finish-tag img-attributes predicate stream))))
	(format stream "</~A>" container-tag-name))))))

