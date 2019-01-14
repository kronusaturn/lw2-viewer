(uiop:define-package #:lw2.components
  (:use #:cl #:alexandria #:lw2.utils)
  (:export
   #:standard-component #:http-args #:prepare-function
   #:make-binding-form
   #:wrap-http-bindings #:wrap-prepare-code
   #:find-component #:delete-component #:define-component #:renderer
   #:component-value-bind))

(in-package #:lw2.components)

(defvar *components* nil)

(defclass standard-component ()
  ((http-args :accessor http-args :initarg :http-args)
   (prepare-function :accessor prepare-function :initarg :prepare-function :type function)))

(defun make-binding-form (additional-vars body &aux var-bindings additional-declarations additional-preamble)
  (loop for x in additional-vars
     when (not (eq (first (ensure-list x)) '*))
     do
       (destructuring-bind (name &key member type default required (request-type :get) (real-name (string-downcase name)) passthrough) (ensure-list x)
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
		    `(let ((sym (find-symbol (string-upcase ,inner-form) ,(find-package '#:keyword))))
		       (if (member sym ,member) sym)))
		   ((and type (subtypep type 'integer))
		    `(let ((,name ,inner-form))
		       (declare (type (or null simple-string) ,name))
		       (if ,name (parse-integer ,name))))
		   (t inner-form)))
		(inner-form
		 (if default
		     `(or ,inner-form ,default)
		     inner-form)))
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
  `(let ,(nreverse var-bindings) (declare ,.(nreverse additional-declarations)) ,.(nreverse additional-preamble) ,@body))

(defmethod wrap-http-bindings ((component standard-component) body)
  (make-binding-form (http-args component) body))

(defmethod wrap-prepare-code ((component standard-component) lambda-list body)
  (with-gensyms (renderer-callback)
    `(lambda (,renderer-callback ,@lambda-list)
       (macrolet ((renderer ((&rest lambda-list) &body body) `(funcall ,',renderer-callback (lambda ,lambda-list ,@body))))
         ,(wrap-http-bindings component body)))))

(defun find-component (name)
  (or (second (find name *components* :key #'car))
      (error "Undefined component: ~A" name)))

(defun delete-component (name)
  (setf *components* (delete name *components* :key #'car)))

(defmacro define-component (name lambda-list options &body body)
  (let* ((class 'standard-component)
         (instance-args
           (map-plist (lambda (key val)
                        (case key
                          (:class
                            (setf class val)
                            nil)
                          (t (list key val))))
                      options)))
    `(progn
       (let ((component
               (make-instance ',class
                              ,@instance-args)))
         (setf (prepare-function component) (compile nil (wrap-prepare-code component ',lambda-list ',body)))
         (delete-component ',name)
         (push (list ',name component) *components*)))))

(defmacro component-value-bind ((&rest binding-forms) &body body)
  (let ((output-form `(progn ,@body)))
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
