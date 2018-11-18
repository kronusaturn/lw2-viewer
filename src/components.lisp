(uiop:define-package #:lw2.components
  (:use #:cl #:alexandria #:lw2.utils)
  (:export
    #:standard-component #:http-args #:prepare-function
    #:wrap-http-bindings #:wrap-prepare-code
    #:find-component #:delete-component #:define-component #:renderer
    #:component-value-bind))

(in-package #:lw2.components)

(defvar *components* nil)

(defclass standard-component ()
  ((http-args :accessor http-args :initarg :http-args)
   (prepare-function :accessor prepare-function :initarg :prepare-function :type function)))

(defmethod wrap-http-bindings ((component standard-component) body)
  (let ((binding-forms
          (loop for arg in (http-args component)
                collect (destructuring-bind (name &rest ign) (if (atom arg) (list arg) arg)
                          (declare (ignore ign))
                          `(,name (hunchentoot:get-parameter (string-downcase ',name)))))))
    `(let ,binding-forms ,@body)))

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
        (destructuring-bind (name &rest args) (if (atom prepare-form) (list prepare-form) prepare-form)
          (let ((binding-vars (if (atom binding-vars) (list binding-vars) binding-vars)))
            (setf output-form
                  `(let ((,(or as name) nil))
                     (multiple-value-bind ,binding-vars (funcall (load-time-value (prepare-function (find-component ',name)))
                                                                 (lambda (renderer) (setf ,(or as name) renderer))
                                                                 ,@args)
                       ,output-form)))))))
    output-form))
