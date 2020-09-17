(uiop:define-package #:lw2.macro-utils
  (:documentation "Facilities for working with macros.")
  (:use #:cl #:iterate #:trivial-cltl2)
  (:import-from #:introspect-environment #:compiler-macroexpand #:compiler-macroexpand-1)
  (:export #:compiler-constantp #:macroexpand-both-1 #:macroexpand-both
	   #:eval-in-environment
	   #:augment-macros #:macro-as-lambda #:macro-list-as-lambdas))

(in-package #:lw2.macro-utils)

(defun compiler-constantp (form &optional environment)
  "Like CONSTANTP, but also try expanding compiler macros."
  (or (constantp form environment)
      (constantp (compiler-macroexpand form environment) environment)))

(defun macroexpand-both-1 (form &optional environment)
  (multiple-value-bind (result expandedp) (macroexpand-1 form environment)
    (multiple-value-bind (result c-expandedp) (compiler-macroexpand-1 result environment)
      (values result (or expandedp c-expandedp)))))

(defun macroexpand-both (form &optional environment)
  (let (result expandedp any-expandedp)
    (iter
     (multiple-value-setq (result expandedp) (macroexpand-both-1 form environment))
     (while expandedp)
     (setf any-expandedp t))
    (values result any-expandedp)))

(defun eval-in-environment (form &optional environment)
  (funcall (enclose `(lambda () ,form) environment)))

(defun augment-macros (environment macro-bindings)
  "Add a set of MACROLET-style macro bindings to an environment."
  (let ((macro-list (iter (for (name args . body) in macro-bindings)
			  (for macro-lambda = (parse-macro name args body))
			  (for macro-fn = (enclose macro-lambda environment))
			  (collect (list name macro-fn)))))
    (augment-environment environment :macro macro-list)))

(defmacro macro-as-lambda (name args &body body)
  "Create a macro expander function as a lambda form."
  (parse-macro name args body))

(defmacro macro-list-as-lambdas (&rest clauses)
  "Create a list of macro expander functions as lambda forms. This is
suitable for passing to AUGMENT-ENVIRONMENT."
  `(list
    ,@(iter (for (name args . body) in clauses)
	    (collect `(list ',name ,(parse-macro name args body))))))
