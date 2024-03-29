(uiop:define-package #:lw2.components
  (:use #:cl #:alexandria #:lw2.utils #:lw2.csrf)
  (:export
   #:standard-component #:prepare-function
   #:make-binding-form
   #:with-http-args
   #:&without-csrf-check
   #:wrap-prepare-code
   #:find-component #:delete-component #:define-component #:renderer
   #:component-value-bind)
  (:unintern
   #:standard-component #:wrap-http-bindings))

(in-package #:lw2.components)

(defvar *components* nil)

(defun make-binding-form (additional-vars body &aux var-bindings additional-declarations additional-preamble)
  (loop for x in additional-vars
     when (not (member (first (ensure-list x)) '(* &without-csrf-check)))
     do
       (destructuring-bind (name &key member type default required (request-type '(:post :get)) (real-name (string-downcase name)) passthrough)
	   (ensure-list x)
	 (let* ((inner-form
		 (if passthrough
		     name
		     `(or ,.(mapcar (lambda (rt)
				      (list (if (eq rt :post) 'hunchentoot:post-parameter 'hunchentoot:get-parameter)
					    real-name))
				    (ensure-list request-type)))))
		(inner-form
		 (cond
		   (member
		    `(let* ((raw-value ,inner-form)
			    (sym (find-symbol (string-upcase raw-value) ,(find-package '#:keyword))))
		       (when raw-value
			 (if (member sym ,member)
			     sym
			     (error "The ~A parameter has an unrecognized value." ',name)))))
		   ((and type (subtypep type 'integer))
		    `(let ((,name ,inner-form))
		       (declare (type (or null simple-string) ,name))
		       (if ,name (parse-integer ,name))))
		   (t inner-form)))
		(inner-form
		 (if (eq type 'boolean)
		     `(let ((,name ,inner-form))
			(if ,name
			    (truthy-string-p ,name)
			    ,default))
		     (if default
			 `(or ,inner-form ,default)
			 inner-form))))
	   (when required
	     (push `(unless (and ,name (not (equal ,name ""))) (error "Missing required parameter: ~A" ,real-name))
		   additional-preamble))
	   (if member
	       (if type (error "Cannot specify both member and type.")
		   (push `(type (or null symbol) ,name) additional-declarations))
	       (if type
		   (push `(type (or null ,type) ,name) additional-declarations)
		   (push `(type (or null simple-string) ,name) additional-declarations)))
	   (when inner-form
	     (push `(,name ,inner-form) var-bindings)))))
  `(progn
     ,@(unless (member '&without-csrf-check additional-vars)
	 '((check-csrf)))
     (let ,(nreverse var-bindings)
       (declare ,.(nreverse additional-declarations))
       ,.(nreverse additional-preamble)
       (block nil ,@body))))

(defmacro with-http-args (http-args &body body)
  (make-binding-form http-args body))

(defun wrap-prepare-code (http-args lambda-list body)
  (with-gensyms (renderer-callback)
    `(lambda (,renderer-callback ,@lambda-list)
       (macrolet ((renderer ((&rest lambda-list) &body body)
		    `(funcall ,',renderer-callback (lambda ,lambda-list (block nil (locally ,@body))))))
         ,(make-binding-form http-args body)))))

(defun find-component (name)
  (or (second (find name *components* :key #'car))
      (error "Undefined component: ~A" name)))

(defun delete-component (name)
  (setf *components* (delete name *components* :key #'car)))

(defmacro define-component (name lambda-list (&key http-args) &body body)
  `(progn
     (let ((component
	    (alist :prepare-function ,(wrap-prepare-code http-args lambda-list body))))
       (delete-component ',name)
       (push (list ',name component) *components*))))

(defun prepare-function (component)
  (cdr (assoc :prepare-function component)))

(defmacro component-value-bind ((&rest binding-forms) &body body)
  (let ((output-form `(locally ,@body)))
    (dolist (b (reverse binding-forms))
      (destructuring-bind (binding-vars prepare-form &key as) b
        (destructuring-bind (name &rest args) (ensure-list prepare-form)
          (let ((binding-vars (ensure-list binding-vars)))
            (setf output-form
                  `(let ((,(or as name) nil))
                     (multiple-value-bind ,binding-vars (funcall (load-time-value (prepare-function (find-component ',name)))
                                                                 (lambda (renderer) (setf ,(or as name) renderer))
                                                                 ,@args)
                       ,output-form)))))))
    output-form))
