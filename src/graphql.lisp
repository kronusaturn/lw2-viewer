(uiop:define-package #:lw2.graphql
  (:documentation "Contains generic GraphQL client functionality required by lw2-viewer.")
  (:use #:cl #:alexandria #:iterate #:lw2.macro-utils)
  (:import-from #:trivial-macroexpand-all #:macroexpand-all)
  (:import-from #:trivial-cltl2 #:enclose #:augment-environment)
  (:export #:+graphql-timestamp-format+ #:write-graphql-simple-field-list #:graphql-query-string* #:graphql-query-string #:graphql-operation-string #:graphql-mutation-string #:timestamp-to-graphql)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.graphql)

(defconstant +graphql-timestamp-format+ (if (boundp '+graphql-timestamp-format+) (symbol-value '+graphql-timestamp-format+)
                                            (substitute-if '(:msec 3) (lambda (x) (and (listp x) (eq (car x) :usec))) local-time:+iso-8601-format+)))

(defmacro declaim-grammar (name)
  ;; Forward declare a grammar form so it can be used before it is defined.
  (let ((write-function (symbolicate '#:write- name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (ftype function ,write-function)
		(notinline ,write-function))
       (pushnew ',name *grammars*))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *grammars* nil)

  (defun writer-macros (stream)
    ;; Return the list of macro expanders that should be active to create a writer.
    (labels ((grammar-writer-macro (grammar)
	       (list grammar
		     (macro-as-lambda grammar (&rest args) `(,(symbolicate '#:write- grammar) ,@args ,stream)))))
      (append (macro-list-as-lambdas
	       (emit-string (&body body)
		 `(write-string (progn ,@body) ,stream))
	       (emit (&body body)
		 `(progn ,@(map 'list (lambda (f) (gen-writer f stream)) body)))
	       (separated-list (type separator list)
		 `(iter (for x in ,list)
			(unless (first-time-p)
			  ,(gen-writer separator stream))
			,(gen-writer `(,type x) stream)))
	       (with-stream ((stream-binding) &body body)
		 `(let ((,stream-binding ,stream))
		    ,@body)))
	      (iter (for grammar in *grammars*)
		    (collect (grammar-writer-macro grammar))))))
  
  (defun gen-writer (form stream &optional env)
    ;; Convert a defgrammar form to a lisp form.
    (etypecase form
      (string `(write-string ,form ,stream))
      (list
       (macroexpand-all
	form
	(augment-environment env :macro (writer-macros stream))))))

  (defun writer-compiler-form (write-function args stream env whole)
    (if (every (lambda (x) (compiler-constantp x env)) args)
	(let ((out-string
	       (with-output-to-string (c-stream)
		 (funcall (enclose `(lambda (c-stream)
				      (declare (notinline ,write-function))
				      (,write-function ,@args c-stream))
				   env)
			  c-stream))))
	  `(write-string
	    ,out-string
	    ,stream))
	whole)))

(defmacro defgrammar (name args &body body)
  (with-gensyms (stream)
    (let ((write-function (symbolicate '#:write- name)))
      (pushnew name *grammars*)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (pushnew ',name *grammars*)
	 (defun ,write-function (,@args ,stream)
	   (declare (notinline ,write-function))
	   ,(gen-writer (cons 'progn body) stream)
	   nil)
	 (define-compiler-macro ,write-function (&whole whole ,@args ,stream &environment env)
	   (writer-compiler-form ',write-function (list ,@args) ,stream env whole))))))

;;; See the GraphQL spec, https://spec.graphql.org/June2018/

(defgrammar graphql-name (obj)
  (emit-string (etypecase obj
		 (string obj)
		 (symbol (json:lisp-to-camel-case (string obj))))))

(declaim-grammar graphql-simple-field-list)

(defgrammar graphql-simple-field (field)
  (typecase field
    (atom (graphql-name field))
    (list (emit (graphql-name (first field))
		(graphql-simple-field-list (rest field))))))

(defgrammar graphql-simple-field-list (fields)
  (emit "{" (separated-list graphql-simple-field "," fields) "}"))

(declaim-grammar graphql-argument)

(defgrammar graphql-value (value)
  (typecase value
	      ((member t) (emit "true"))
	      ((member nil) (emit "false"))
	      ((member :null) (emit "null"))
	      ((member :undefined) (emit "undefined"))
	      (symbol (graphql-name value))
	      ((cons (member :list) list)
	       (emit "[" (separated-list graphql-value "," (rest value)) "]"))
	      ((cons list list)
	       (emit "{" (separated-list graphql-argument "," value) "}"))
	      (t (with-stream (stream) (json:encode-json value stream)))))

(defgrammar graphql-argument (cons)
  (emit (graphql-name (car cons))
	":"
	(graphql-value (cdr cons))))

(defgrammar graphql-argument-alist (list)
  (when list
    (emit "(" (separated-list graphql-argument "," list) ")")))

(declaim-grammar graphql-field)

(defgrammar graphql-combined-fields (fields simple-fields)
  (when (or fields simple-fields)
    (emit "{" (separated-list graphql-field "," fields))
    (when (and fields simple-fields) (emit ","))
    (emit (separated-list graphql-simple-field "," simple-fields) "}")))

(defgrammar graphql-field (field)
  (destructuring-bind (name &key args fields simple-fields) field
    (emit (graphql-name name))
    (when args
      (graphql-argument-alist args))
    (graphql-combined-fields fields simple-fields)))

(defgrammar graphql-field-list (fields)
  (emit "{" (separated-list graphql-field "," fields) "}"))

(defgrammar graphql-operation (operation-type name variable-definitions fields simple-fields)
  (emit (graphql-name operation-type) " "
	(graphql-name name)
	(graphql-argument-alist variable-definitions)
	(graphql-combined-fields fields simple-fields)))

(defgrammar graphql-simple-query (query-type terms fields)
  (emit (graphql-name query-type)
	(graphql-argument-alist terms)
	(graphql-simple-field-list fields)))

(defun graphql-query-string* (query-type terms fields)
  (with-output-to-string (stream)
    (write-graphql-simple-query query-type terms fields stream)))

(defun graphql-query-string (query-type terms fields)
  (with-output-to-string (stream)
    (write-string "{" stream)
    (write-graphql-simple-query query-type terms fields stream)
    (write-string "}" stream)))

(defun graphql-operation-string (operation-type query-type terms fields)
  (with-output-to-string (stream)
    (write-graphql-name operation-type stream)
    (write-string "{" stream)
    (write-graphql-simple-query query-type terms fields stream)
    (write-string "}" stream)))

(defun graphql-mutation-string (mutation-type terms fields)
  (format nil "mutation ~A{~A}" mutation-type (graphql-query-string* mutation-type terms fields)))

(defun timestamp-to-graphql (timestamp)
  (local-time:format-timestring nil timestamp
				:format lw2.graphql:+graphql-timestamp-format+
				:timezone local-time:+utc-zone+))
