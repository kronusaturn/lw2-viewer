(uiop:define-package #:lw2.schema-type
  (:use #:cl #:lw2.utils)
  (:export #:define-schema-type #:undefine-schema-type #:find-schema-type #:schema-bind))

(in-package #:lw2.schema-type)

(defvar *schema-types* nil)

(defun delete-schema-type (name)
  (setf *schema-types* (delete name *schema-types* :key #'car)))

(defmacro undefine-schema-type (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (delete-schema-type ,name)))

(defmacro define-schema-type (name options fields)
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (delete-schema-type ,name)
     (setf *schema-types*
	   (acons ,name (alist :fields ',fields)
		  *schema-types*))))

(defun find-schema-type (schema-type-name)
  (let ((c (assoc schema-type-name *schema-types*)))
    (if c
	(cdr c)
	(error "Undefined schema-type: ~A" schema-type-name))))

(defmacro schema-bind ((schema-type-name datum bindings &key context) &body body)
  (let* ((schema-type (find-schema-type schema-type-name))
	 (fields (cdr (assoc :fields schema-type))))
    `(alist-bind
      ,(loop with added = (make-hash-table :test 'eq)
	  for type-field in fields
	  nconc (destructuring-bind (binding-sym type &key alias ((:context field-context)) &allow-other-keys) type-field
		  (when (and (not (gethash binding-sym added))
			     (if (eq bindings :auto)
				 (or (not field-context) (eq field-context context))
				 (member binding-sym bindings :test #'string=)))
		    (setf (gethash binding-sym added) t)
		    (list (list* (intern (string binding-sym) *package*)
				 (if (eq type 'string) 'simple-string type) ; Optimization, assuming strings coming from the backend
					                                    ; can never be displaced etc.
				 (if alias (list alias)))))))
      ,datum
      ,@body)))
