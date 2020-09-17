(uiop:define-package #:lw2.utils
  (:use #:cl #:alexandria #:iterate
	#:lw2.macro-utils)
  (:export #:nalist #:nalist* #:alist #:alist*
	   #:alist-without-null #:alist-without-null*
	   #:dynamic-let #:dynamic-let* #:dynamic-flet #:dynamic-labels
	   #:get-unix-time #:substring #:regex-replace-body #:reg #:match
	   #:to-boolean #:nonzero-number-p #:truthy-string-p
	   #:firstn #:map-plist #:filter-plist #:alist-bind
	   #:list-cond #:list-cond*
	   #:string-to-existing-keyword #:call-with-safe-json
	   #:delete-easy-handler #:abnormal-unwind-protect
	   #:ignorable-multiple-value-bind
	   #:compare-streams)
  (:recycle #:lw2-viewer))

(in-package #:lw2.utils)

(defun nalist (&rest params) (plist-alist params))

(defun nalist* (&rest params)
  (nconc (plist-alist (butlast params))
	 (car (last params))))

(defun inner-make-alist (params &optional (env nil env-p))
  (iter
   (for (key val) on params by #'cddr)
   (collect (if (and env-p (compiler-constantp key env) (compiler-constantp val env))
		`'(,(eval-in-environment key env) . ,(eval-in-environment val env))
		`(cons ,key ,val)))))

(define-compiler-macro nalist (&rest params)
  `(list ,.(inner-make-alist params)))

(define-compiler-macro nalist* (&rest params)
  `(list*
    ,.(inner-make-alist (butlast params))
    ,(car (last params))))

(declaim (ftype function alist alist*))
(setf (fdefinition 'alist) (fdefinition 'nalist)
      (fdefinition 'alist*) (fdefinition 'nalist*))

(define-compiler-macro alist* (&rest params &environment env)
  `(list*
    ,.(inner-make-alist (butlast params) env)
    ,(car (last params))))

(define-compiler-macro alist (&rest params &environment env)
  (let ((count (length params))
	(reverse (reverse params))
	(constant-list nil)
	(constant-count 0))
    (iter (for (val key) on reverse by #'cddr)
	  (cond ((and (compiler-constantp key env) (compiler-constantp val env))
		 (push (cons (eval-in-environment key env) (eval-in-environment val env)) constant-list)
		 (incf constant-count))
		(t (finish))))
    (if (= (* constant-count 2) count)
	`(quote ,constant-list)
	`(alist* ,@(butlast params (* constant-count 2)) (quote ,constant-list)))))

(defun remove-alist-nulls (alist)
  (remove-if (lambda (x) (null (cdr x))) alist))

(defun alist-without-null (&rest params)
  (remove-alist-nulls (plist-alist params)))

(defun alist-without-null* (&rest params)
  (list* (remove-alist-nulls (plist-alist (butlast params))) (car (last params))))

(define-compiler-macro alist-without-null (&rest params)
  `(list-cond
    ,@(iter (for (key val) on params by #'cddr)
	    (collect `(,val ,key ,val)))))

(define-compiler-macro alist-without-null* (&rest params)
  `(list-cond*
    ,@(iter (for (key val) on (butlast params) by #'cddr)
	    (collect `(,val ,key ,val)))
    ,(car (last params))))

(defun dynamic-let-inner (initial functionp clauses body)
  `(,initial ,clauses
	     (declare (dynamic-extent ,@(iter (for c in clauses)
					      (collect (if functionp `(function ,(first c)) (first c))))))
     ,@body))

(defmacro dynamic-let ((&rest clauses) &body body)
  (dynamic-let-inner 'let nil clauses body))

(defmacro dynamic-let* ((&rest clauses) &body body)
  (dynamic-let-inner 'let* nil clauses body))

(defmacro dynamic-flet ((&rest clauses) &body body)
  (dynamic-let-inner 'flet t clauses body))

(defmacro dynamic-labels ((&rest clauses) &body body)
  (dynamic-let-inner 'labels t clauses body))

(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(deftype array-dimension-type () `(integer 0 ,(- array-dimension-limit 1)))

(declaim (inline substring)
         (ftype (function (string array-dimension-type &optional array-dimension-type) (and string (not simple-string))) substring))
(defun substring (string start &optional (end (length string)))
  (make-array (- end start) :element-type 'character :displaced-to string :displaced-index-offset start))

(defmacro regex-replace-body ((regex target &rest args) &body body)
  `(ppcre:regex-replace-all
    ,regex ,target
    (lambda (target-string start end match-start match-end reg-starts reg-ends)
      (declare (ignore start end)
	       (type string target-string)
	       (type array-dimension-type match-start match-end)
	       (type vector reg-starts reg-ends))
      (let ((reg-count (length reg-starts)))
	(labels ((reg (n) (when (> reg-count n)
			    (when-let ((start (aref reg-starts n)))
			      (substring target-string start (aref reg-ends n)))))
		 (match () (substring target-string match-start match-end)))
	  (declare (dynamic-extent #'reg #'match))
	  ,@body)))
    ,@args))

(declaim (inline to-boolean))
(defun to-boolean (value)
  (and value t))

(declaim (inline nonzero-number-p))
(defun nonzero-number-p (value)
  (and (typep value 'number)
       (/= 0 value)))

(defun truthy-string-p (string)
  (and (typep string 'string)
       (to-boolean (member string '("t" "true" "y" "yes" "1") :test #'string-equal))))

(defun firstn (list n)
  (loop for x in list
	for i from 1 to n
	collect x))

(defun map-plist (fn plist)
  (loop for (key val . rest) = plist then rest
        while key
        nconc (funcall fn key val)))

(defun filter-plist (plist &rest args)
  (declare (dynamic-extent args))
  (map-plist (lambda (key val) (when (member key args) (list key val))) plist))

(defmacro alist-bind (bindings alist &body body)
  "Binds elements of ALIST so they can be used as if they were lexical variables.

Syntax: alist-bind (binding-entry*) alist forms*
=> result*
binding-entry ::= (variable-name &optional type alist-key)

Each VARIABLE-NAME is bound to the corresponding datum in ALIST. Modifying these
bindings with SETF will also update the ALIST.
TYPE: type designator, not evaluated.
ALIST-KEY: the alist key, as in the first argument to ASSOC. If it is not
specified, the KEYWORD symbol with the same name as VARIABLE-NAME is used."
  (once-only (alist)
    (let ((inner-bindings (loop for x in bindings collect
                                (destructuring-bind (bind &optional type key) (if (consp x) x (list x))
                                  (list (gensym (string bind)) (gensym (string bind)) (gensym (string bind)) bind (or type t) (or key (intern (string bind) '#:keyword)))))))
      (macrolet ((inner-loop (&body body)
		   `(loop for (fn-gensym cons-gensym value-gensym bind type key) in inner-bindings collect
			 (progn fn-gensym cons-gensym value-gensym bind type key ,@body))))
	`(let (,@(inner-loop cons-gensym))
	   (declare (type list ,@(inner-loop cons-gensym)))
	   (loop for elem in ,alist do
		(case (car elem)
		  ,@(inner-loop `(,key (unless ,cons-gensym (setf ,cons-gensym elem))))))
	   (let (,@(inner-loop `(,value-gensym (cdr ,cons-gensym))))
	     (declare ,@(inner-loop `(type ,type ,value-gensym)))
	     (flet (,@(inner-loop `(,fn-gensym () ,value-gensym))
		    ,@(inner-loop `((setf ,fn-gensym) (new) (setf ,value-gensym new ,cons-gensym (cons ,key new) ,alist (cons ,cons-gensym ,alist)))))
	       (declare (inline ,@(inner-loop fn-gensym)))
	       (symbol-macrolet ,(inner-loop `(,bind (,fn-gensym)))
		 ,@body))))))))

(defmacro list-cond* (&body clauses)
  (labels ((expand (clauses)
	     (if (endp (rest clauses))
		 (first clauses)
		 (destructuring-bind (predicate-form data-form &optional value-form) (first clauses)
		   (with-gensyms (predicate rest)
		     `(let* ((,predicate ,predicate-form)
			     (,rest ,(expand (rest clauses))))
			(declare (dynamic-extent ,predicate))
			(if ,predicate
			    (cons ,(if value-form `(cons ,data-form ,value-form) data-form)
				  ,rest)
			    ,rest)))))))
    (expand clauses)))

(defmacro list-cond (&body clauses)
  `(list-cond* ,@clauses nil))

(defun string-to-existing-keyword (string)
  (or (find-symbol (json:camel-case-to-lisp string) (find-package '#:keyword))
      string))

(defun call-with-safe-json (fn)
  (let ((json:*json-identifier-name-to-lisp* #'identity)
	(json:*identifier-name-to-key* #'string-to-existing-keyword))
    (funcall fn)))

(defun delete-easy-handler (name)
  (setf hunchentoot::*easy-handler-alist*
	(remove name hunchentoot::*easy-handler-alist* :key #'third)))

(defmacro abnormal-unwind-protect (protected-form &body body)
  (alexandria:with-gensyms (normal-return)
    `(let ((,normal-return nil))
       (unwind-protect
	    (multiple-value-prog1
		,protected-form
	      (setf ,normal-return t))
	 (unless ,normal-return
	   ,@body)))))

(defmacro ignorable-multiple-value-bind ((&rest bindings) value-form &body body)
  (let (new-bindings ignores)
    (dolist (binding (reverse bindings))
      (if (eq binding '*)
	  (let ((gensym (gensym)))
	    (push gensym new-bindings)
	    (push gensym ignores))
	  (push binding new-bindings)))
    `(multiple-value-bind ,new-bindings ,value-form
	 (declare (ignore ,.ignores))
       ,@body)))

(defgeneric unwrap-stream (s)
  (:method ((s stream)) nil)
  (:method ((s flex:flexi-stream)) (flex:flexi-stream-stream s))
  (:method ((s chunga:chunked-stream)) (chunga:chunked-stream-stream s)))

(defun compare-streams (a b)
  (if (eq a b)
      t
      (or
       (if-let (u-a (unwrap-stream a))
	       (compare-streams u-a b))
       (if-let (u-b (unwrap-stream b))
	       (compare-streams a u-b)))))
