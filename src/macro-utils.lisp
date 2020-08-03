(uiop:define-package #:lw2.macro-utils
  (:documentation "Facilities for working with macros.")
  (:use #:cl #:iterate #:trivial-cltl2)
  (:export #:compiler-constantp #:augment-macros #:macro-as-lambda #:macro-list-as-lambdas))

(in-package #:lw2.macro-utils)

(defun compiler-constantp (form &optional environment)
  "Like CONSTANTP, but also try expanding compiler macros."
  (or (constantp form environment)
      (constantp (introspect-environment:compiler-macroexpand form environment) environment)))

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
