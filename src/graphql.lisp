(uiop:define-package #:lw2.graphql
  (:documentation "Contains generic GraphQL client functionality required by lw2-viewer.")
  (:use #:cl #:alexandria)
  (:export #:graphql-query-string* #:graphql-query-string)
  (:recycle #:lw2.backend #:lw2.login))

(in-package #:lw2.graphql)

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
					  (list (format nil "{~{~A~^,~}}" (terms v)))
					  (t (format nil "~S" v))))))
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

