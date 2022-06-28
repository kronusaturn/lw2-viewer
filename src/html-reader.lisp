(uiop:define-package #:lw2.html-reader
  (:use #:cl #:alexandria #:iterate #:named-readtables)
  (:export #:*html-output* #:with-html-stream-output #:safe #:encode-entities #:html-reader)
  (:recycle #:lw2-viewer))

(in-package #:lw2.html-reader)

(defvar *html-output* nil)

(defun encode-entities (text &optional stream)
  #||(if-client
   (let ((output
	  (ps:chain text (replace (ps:regex "/[<>\"'&]/g")
				  (lambda (match)
				    (concatenate 'string
						 "&"
						 (ps:getprop (ps:create "<" "lt"
									">" "gt"
									"\"" "quot"
									"'" "apos"
									"&" "amp")
							     match)
						 ";"))))))
     (if stream
	 (progn (ps:chain stream (push output)) nil)
	 output))|#
   (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (plump:encode-entities (princ-to-string text) stream)));)

(trivial-cltl2:define-declaration html-output (decl env) (declare (ignore env)) (values :declare (cons 'html-output (second decl))))

(defmacro with-html-stream-output (&environment env &body body)
  (let ((body (trivia:match (first body)
			    ((list :stream stream-var)
			     `((let ((,stream-var html-output)) ,@(rest body))))
			    (_ body))))
    (if (trivial-cltl2:declaration-information 'html-output env)
	`(progn ,@body nil)
	`(let ((html-output *html-output*))
	   (declare (ignorable html-output)
		    (html-output html-output))
	   ,@body
	   nil))))

(defun process-html-stream-output-forms (forms)
  (iter (for form in forms)
	(collect (trivia:match form
			       ((list* :stream _) nil)
			       ((list* (or 'write-string 'princ) string _) `(ps:chain html-output (push ,string)))
			       ((list* 'encode-entities _) form)
			       ((list* 'with-html-stream-output forms) `(progn ,@(process-html-stream-output-forms forms)))
			       (_ `(ps:chain html-output (push ,form)))))))

(ps:defpsmacro with-html-stream-output (&body body)
  `(let ((html-output (make-array)))
     (macrolet ((write-string (string stream)
		  (declare (ignore stream))
		  `(ps:chain html-output (push ,string)))
		(princ (string stream)
		  (declare (ignore stream))
		  `(ps:chain html-output (push ,string)))
		(with-html-stream-output (&body body)
		  `(progn ,@(process-html-stream-output-forms body) nil)))
       ,@(process-html-stream-output-forms body))
     (ps:chain html-output (join ""))))

(defun html-reader (stream char)
  (declare (ignore char))
  (let (element
	out-body
	(string-output "")
	(buffer (make-array 128
			    :element-type 'character
			    :adjustable t
			    :fill-pointer 0)))
    (labels ((output-strings (&rest strings)
	       (setf string-output (apply #'concatenate 'simple-string string-output strings)))
	     (flush-output ()
	       (unless (string= string-output "")
		 (appendf out-body `((write-string ,string-output html-output)))
		 (setf string-output "")))
	     (output-read-object ()
	       (let ((object (read-preserving-whitespace stream)))
		 (multiple-value-bind (safe object)
		     (if (and (consp object) (eq (first object) 'safe))
			 (values t (second object))
			 (values nil object))
		   (cond
		     ((and (consp object) (stringp (first object)))
		      (flush-output)
		      (appendf out-body
			       (if safe
				   `((format html-output ,@object))
				   `((encode-entities (format nil ,@object) html-output)))))
		     ((and (consp object) (eq (first object) 'with-html-stream-output))
		      (flush-output)
		      (appendf out-body (list object)))
		     ((and (constantp object) (or (stringp object) (numberp object)))
		      (output-strings (princ-to-string (eval object))))
		     (t
		      (flush-output)
		      (appendf out-body
			       (if safe
				   `((princ ,object html-output))
				   `((encode-entities (or ,object "") html-output))))))))))
	    (loop for c = (peek-char nil stream)
		  while (not (member c '(#\Space #\Newline #\>)))
	       do (vector-push-extend (read-char stream) buffer))
	    (when-let ((symbol (find-symbol (concatenate 'string "<" buffer) *package*)))
	      (unless (eq (symbol-package symbol) *package*)
		(return-from html-reader symbol)))
	    (setf element (coerce buffer 'simple-string))
	    (output-strings "<" element)
	    (loop
	       with need-whitespace = t
	       with in-leading-whitespace = nil
	       with in-tag = t
	       for c = (read-char stream)
	       when (eq c #\Newline) do (setf in-leading-whitespace t)
	       else when (not (member c '(#\Space #\Tab))) do (setf in-leading-whitespace nil)
	       when (eq c #\>)
	       do (progn
		    (output-strings ">")
		    (if (member element '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr")
				:test #'string-equal)
			(return nil)
			(setf need-whitespace nil
			      in-tag nil)))
	       else when (and in-tag (eq c #\=))
	       do (progn
		    (output-strings "=\"")
		    (let ((*readtable* (find-readtable 'html-reader-inner)))
		      (output-read-object))
		    (output-strings "\""))
	       else when (eq c #\<)
	       do (cond
		    ((eq (peek-char nil stream) #\/)
		     (read-char stream)
		     (unless (and
			      (loop for x across element
				 when (not (eq (read-char stream) x))
				 return nil
				 finally (return t))
			      (eq (read-char stream) #\>))
		       (error "Mismatched HTML tag: ~A at position ~A." element (file-position stream)))
		     (output-strings "</" element ">")
		     (return nil))
		    (t
		     (unread-char c stream)
		     (output-read-object)))
	       else when (eq c #\\)
	       do (output-strings (string (read-char stream)))
	       else when (eq c #\()
	       do (progn
		    (unread-char c stream)
		    (output-read-object))
	       else when (member c '(#\Space #\Newline #\Tab))
	       do (when (or need-whitespace (not in-leading-whitespace))
		    (output-strings " ")
		    (setf need-whitespace nil))
	       else do (progn
			 (output-strings (string c))
			 (setf need-whitespace t)))
	    (flush-output)
	    `(with-html-stream-output ,.out-body))))

(defreadtable html-reader
  (:merge :standard)
  (:macro-char #\< #'html-reader t)
  #|(:macro-char #\" #'(lambda (stream char)
		       (let ((*readtable* (find-readtable :standard)))
			 (funcall (get-macro-character #\" *readtable*) stream char))))|#)

(defreadtable html-reader-inner
  (:merge html-reader)
  (:macro-char #\> #'(lambda (stream char) (declare (ignore stream char)) (values))))
		       
