(uiop:define-package #:lw2.json
  (:use #:cl)
  (:export #:encode #:encode-to-string #:decode #:safe-decode))

(in-package #:lw2.json)

(defun string-to-existing-keyword (string)
  (or (find-symbol (json:camel-case-to-lisp string) (find-package '#:keyword))
      string))

(defun alistp (list)
  (loop
     for elem in list
     unless (consp elem)
     return nil
     unless (atom (car elem))
     return nil
     finally (return t)))

(defun encode-list-guessing (object stream)
  (if (alistp object)
      (yason:encode-alist object stream)
      (yason:encode-plain-list-to-array object stream)))

(defun encode (object stream)
  (let ((yason:*list-encoder* #'encode-list-guessing)
	(yason:*symbol-key-encoder* (lambda (x) (json:lisp-to-camel-case (string x)))))
    (yason:encode object stream)))

(defun encode-to-string (object)
  (with-output-to-string (stream)
    (encode object stream)))

(defun decode (source)
  (let ((yason:*parse-object-as* :alist)
	(yason:*parse-object-key-fn* #'string-to-existing-keyword))
    (yason:parse source)))

(defun safe-decode (source)
  (values
   (ignore-errors
     (decode source))))
