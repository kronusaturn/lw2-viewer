(uiop:define-package #:lw2.graphql
  (:documentation "Contains generic GraphQL client functionality required by lw2-viewer.")
  (:use #:cl #:alexandria #:iterate)
  (:export #:+graphql-timestamp-format+ #:write-graphql-simple-field-list #:graphql-query-string* #:graphql-query-string #:graphql-mutation-string #:timestamp-to-graphql)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.graphql)

(defconstant +graphql-timestamp-format+ (if (boundp '+graphql-timestamp-format+) (symbol-value '+graphql-timestamp-format+)
                                            (substitute-if '(:msec 3) (lambda (x) (and (listp x) (eq (car x) :usec))) local-time:+iso-8601-format+)))

(defun compiler-constantp (form &optional environment)
  (or (constantp form environment)
      (constantp (introspect-environment:compiler-macroexpand form environment) environment)))

(defmacro declaim-grammar (name)
  `(declaim (ftype function ,(symbolicate '#:write- name))))

(defmacro defgrammar (name args &body body)
  (with-gensyms (stream)
    (labels ((gen-writer (form)
	       (etypecase form
		 (string `(write-string ,form ,stream))
		 (list
		  (case (first form)
		    (to-string
		     `(write-string (progn ,@(rest form)) ,stream))
		    (sequence
		     `(progn ,@(map 'list #'gen-writer (rest form))))
		    (separated-list
		     (destructuring-bind (type separator list) (rest form)
		       `(iter (for x in ,list)
			      (unless (first-time-p)
				,(gen-writer separator))
			      ,(gen-writer `(,type x)))))
		    (typecase
		     (destructuring-bind (obj &rest clauses) (rest form)
		       `(typecase ,obj
			  ,@(iter (for (type body) in clauses)
				  (collect `(,type ,(gen-writer body)))))))
		    (with-stream
		      (destructuring-bind ((stream-binding) &rest body) (rest form)
			`(let ((,stream-binding ,stream))
			   ,@body)))
		    (t
		     (destructuring-bind (type obj) form
		     `(,(symbolicate '#:write- type) ,obj ,stream))))))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,(symbolicate '#:write- name) (,@args ,stream)
	   ,(gen-writer (first body))
	   nil)
	 (define-compiler-macro ,(symbolicate '#:write- name) (&whole whole ,@args ,stream &environment env)
	   (if (every (lambda (x) (compiler-constantp x env)) (list ,@args))
	       `(write-string
		 ,(with-output-to-string (c-stream)
					 (funcall (trivial-cltl2:enclose `(lambda (c-stream) (funcall (symbol-function ',(symbolicate '#:write- ',name)) ,,@args c-stream)) env) c-stream))
		 ,,stream)
	       whole))))))

(defgrammar graphql-name (obj)
  (to-string (etypecase obj
	       (string obj)
	       (symbol (json:lisp-to-camel-case (string obj))))))

(declaim-grammar graphql-simple-field-list)

(defgrammar graphql-simple-field (field)
  (typecase field
    (atom (graphql-name field))
    (list (sequence (graphql-name (first field))
		    (graphql-simple-field-list (rest field))))))

(defgrammar graphql-simple-field-list (fields)
  (sequence "{" (separated-list graphql-simple-field "," fields) "}"))

(declaim-grammar graphql-argument)

(defgrammar graphql-value (value)
  (typecase value
	      ((member t) "true")
	      ((member nil) "false")
	      ((member :null) "null")
	      ((member :undefined) "undefined")
	      ((cons (member :list) list)
	       (sequence "[" (separated-list graphql-value "," (rest value)) "]"))
	      ((cons list list)
	       (sequence "{" (separated-list graphql-argument "," value) "}"))
	      (t (with-stream (stream) (json:encode-json value stream)))))

(defgrammar graphql-argument (cons)
  (sequence (graphql-name (car cons))
	    ":"
	    (graphql-value (cdr cons))))

(defgrammar graphql-argument-alist (list)
  (sequence "(" (separated-list graphql-argument "," list) ")"))

(defun graphql-query-string* (query-type terms fields)
  (with-output-to-string (stream)
    (write-graphql-name query-type stream)
    (write-graphql-argument-alist terms stream)
    (when fields
      (write-graphql-simple-field-list fields stream))))

(defun graphql-query-string (query-type terms fields)
  (format nil "{~A}" (graphql-query-string* query-type terms fields)))

(defun graphql-mutation-string (mutation-type terms fields)
  (format nil "mutation ~A{~A}" mutation-type (graphql-query-string* mutation-type terms fields)))

(defun timestamp-to-graphql (timestamp)
  (local-time:format-timestring nil timestamp
				:format lw2.graphql:+graphql-timestamp-format+
				:timezone local-time:+utc-zone+))
