(uiop:define-package #:lw2.graphql
  (:documentation "Contains generic GraphQL client functionality required by lw2-viewer.")
  (:use #:cl #:alexandria)
  (:export #:+graphql-timestamp-format+ #:graphql-query-string* #:graphql-query-string #:graphql-mutation-string)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.graphql)

(defconstant +graphql-timestamp-format+ (if (boundp '+graphql-timestamp-format+) +graphql-timestamp-format+
                                            (substitute-if '(:msec 3) (lambda (x) (and (listp x) (eq (car x) :usec))) local-time:+iso-8601-format+)))

(defun graphql-query-string* (query-type terms fields)
  (labels ((terms (tlist)
		  (loop for (k . v) in tlist
			when k
			collect (format nil "~A:~A"
					(json:lisp-to-camel-case (string k))
					(typecase v
					  ((member t) "true") 
					  ((member nil) "false")
					  ((member :null) "null")
					  ((member :undefined) "undefined")
					  ((cons list list) (format nil "{~{~A~^,~}}" (terms v)))
					  (t (json:encode-json-to-string v))))))
	   (fields (flist)
		   (map 'list (lambda (x) (typecase x
					    (string x)
					    (symbol (json:lisp-to-camel-case (string x)))
					    (list (format nil "~A{~{~A~^,~}}" (json:lisp-to-camel-case (string (first x))) (fields (rest x))))))
			flist)))
    (format nil "~A(~{~A~^,~})~@[{~{~A~^,~}}~]"
	    query-type
	    (terms terms)
	    (fields fields))))

(defun graphql-query-string (query-type terms fields)
  (format nil "{~A}" (graphql-query-string* query-type terms fields)))

(defun graphql-mutation-string (mutation-type terms fields)
  (format nil "mutation ~A{~A}" mutation-type (graphql-query-string* mutation-type terms fields)))
