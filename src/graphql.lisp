(uiop:define-package #:lw2.graphql
  (:documentation "Contains generic GraphQL client functionality required by lw2-viewer.")
  (:use #:cl #:alexandria #:iterate)
  (:import-from #:trivial-macroexpand-all #:macroexpand-all)
  (:import-from #:trivial-cltl2 #:parse-macro #:enclose #:augment-environment)
  (:export #:+graphql-timestamp-format+ #:write-graphql-simple-field-list #:graphql-query-string* #:graphql-query-string #:graphql-mutation-string #:timestamp-to-graphql)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.graphql)

(defconstant +graphql-timestamp-format+ (if (boundp '+graphql-timestamp-format+) (symbol-value '+graphql-timestamp-format+)
                                            (substitute-if '(:msec 3) (lambda (x) (and (listp x) (eq (car x) :usec))) local-time:+iso-8601-format+)))

(defun compiler-constantp (form &optional environment)
  (or (constantp form environment)
      (constantp (introspect-environment:compiler-macroexpand form environment) environment)))

(defun augment-macros (environment macro-bindings)
  (let ((macro-list (iter (for (name args . body) in macro-bindings)
			  (for macro-lambda = (parse-macro name args body))
			  (for macro-fn = (enclose macro-lambda environment))
			  (collect (list name macro-fn)))))
    (augment-environment environment :macro macro-list)))

(defvar *grammars* nil)

(defmacro declaim-grammar (name)
  `(declaim (ftype function ,(symbolicate '#:write- name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-writer (form stream &optional env)
    (etypecase form
      (string `(write-string ,form ,stream))
      (list
       (macroexpand-all
	form
	(augment-macros
	 env
	 (append
	  `((to-string (&body body)
		       `(write-string (progn ,@body) ,',stream))
	    (sequence (&body body)
		      `(progn ,@(map 'list (lambda (f) (gen-writer f ',stream)) body)))
	    (separated-list (type separator list)
			    `(iter (for x in ,list)
				   (unless (first-time-p)
				     ,(gen-writer separator ',stream))
				   ,(gen-writer `(,type x) ',stream)))
	    (with-stream ((stream-binding) &body body)
	      `(let ((,stream-binding ,',stream))
		 ,@body)))
	  (iter (for grammar in *grammars*)
		(collect `(,grammar (&rest args) `(,(symbolicate '#:write- ',grammar) ,@args ,',stream)))))))))))

(defmacro defgrammar (name args &body body)
  (with-gensyms (stream)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf *grammars* (adjoin ',name *grammars*))
       (defun ,(symbolicate '#:write- name) (,@args ,stream)
	 ,(gen-writer (cons 'progn body) stream)
	 nil)
       (define-compiler-macro ,(symbolicate '#:write- name) (&whole whole ,@args ,stream &environment env)
	 (if (every (lambda (x) (compiler-constantp x env)) (list ,@args))
	     `(write-string
	       ,(with-output-to-string (c-stream)
				       (funcall (trivial-cltl2:enclose `(lambda (c-stream) (funcall (symbol-function ',(symbolicate '#:write- ',name)) ,,@args c-stream)) env) c-stream))
	       ,,stream)
	     whole)))))

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
	      ((member t) (sequence "true"))
	      ((member nil) (sequence "false"))
	      ((member :null) (sequence "null"))
	      ((member :undefined) (sequence "undefined"))
	      (symbol (graphql-name value))
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

(declaim-grammar graphql-field)

(defgrammar graphql-combined-fields (fields simple-fields)
  (when (or fields simple-fields)
    (sequence "{" (separated-list graphql-field "," fields))
    (when (and fields simple-fields) (sequence ","))
    (sequence (separated-list graphql-simple-field "," simple-fields) "}")))

(defgrammar graphql-field (field)
  (destructuring-bind (name &key args fields simple-fields) field
    (sequence (graphql-name name))
    (when args
      (graphql-argument-alist args))
    (graphql-combined-fields fields simple-fields)))

(defgrammar graphql-field-list (fields)
  (sequence "{" (separated-list graphql-field "," fields) "}"))

(defgrammar graphql-operation (operation-type name variable-definitions fields simple-fields)
  (sequence (graphql-name operation-type) " "
	    (graphql-name name)
	    (graphql-argument-alist variable-definitions)
	    (graphql-combined-fields fields simple-fields)))

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
