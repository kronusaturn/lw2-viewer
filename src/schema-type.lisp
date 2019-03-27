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

(defmacro schema-bind ((schema-type-name datum bindings &key qualifier) &body body)
  (let* ((schema-type (find-schema-type schema-type-name))
	 (fields (cdr (assoc :fields schema-type))))
    `(alist-bind
      ,(loop for type-field in fields
	  nconc (destructuring-bind (binding-sym type &key alias ((:qualifier field-qualifier)) &allow-other-keys) type-field
		  (if (if (eq bindings :auto)
			  (or (not field-qualifier) (eq field-qualifier qualifier))
			  (member binding-sym bindings :test #'string=))
		      (list (list* (intern (string binding-sym) *package*) type (if alias (list alias)))))))
      ,datum
      ,@body)))
