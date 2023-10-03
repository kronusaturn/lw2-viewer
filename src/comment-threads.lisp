(uiop:define-package #:lw2.comment-threads
    (:use #:cl #:lw2.utils #:lw2.user-context #:lw2.conditions #:lw2.data-viewers.comment)
  (:import-from #:alexandria #:if-let)
  (:export #:make-comment-parent-hash
	   #:comment-chrono-to-html
	   #:comment-item-to-html
	   #:comment-thread-to-html
	   #:comment-tree-to-html
	   #:sort-items))

(in-package #:lw2.comment-threads)

(defun make-comment-parent-hash-real (comments)
  (let ((existing-comment-hash (make-hash-table :test 'equal))
        (hash (make-hash-table :test 'equal)))
    (dolist (c comments)
      (if-let (id (cdr (assoc :--id c)))
	      (setf (gethash id existing-comment-hash) t)))
    (dolist (c comments)
      (let* ((parent-id (cdr (assoc :parent-comment-id c)))
	     (old (gethash parent-id hash)))
	(setf (gethash parent-id hash) (cons c old))
        (when (and parent-id (not (gethash parent-id existing-comment-hash)))
          (let ((placeholder (alist :--id parent-id :parent-comment-id nil :deleted t)))
            (setf (gethash parent-id existing-comment-hash) t
                  (gethash nil hash) (cons placeholder (gethash nil hash)))))))
    (maphash (lambda (k old)
	       (setf (gethash k hash) (nreverse old)))
	     hash)
    (labels
      ((count-children (parent)
	(let ((children (gethash (cdr (assoc :--id parent)) hash)))
	  (+ (length children) (apply #'+ (map 'list #'count-children children))))) 
       (add-child-counts (comment-list)
	(loop for c in comment-list
	      as id = (cdr (assoc :--id c))
	      do (setf (gethash id hash) (add-child-counts (gethash id hash))) 
	      collecting (cons (cons :child-count (count-children c)) c))))
      (setf (gethash nil hash) (add-child-counts (gethash nil hash)))) 
    hash)) 

(defparameter *comment-parent-hash-cache* (make-hash-table :test 'eq
							   :weakness :value
							   :synchronized t))

(defun make-comment-parent-hash (comments)
  (or (gethash comments *comment-parent-hash-cache*)
      (setf (gethash comments *comment-parent-hash-cache*) (make-comment-parent-hash-real comments))))

(defun comment-thread-to-html (out-stream emit-comment-item-fn)
  (format out-stream "<ul class=\"comment-thread\">")
  (funcall emit-comment-item-fn)
  (format out-stream "</ul>"))

(defun comment-item-to-html (out-stream comment &key extra-html-fn with-post-title level level-invert)
  (with-error-html-block ()
    (let ((c-id (cdr (assoc :--id comment)))
	  (user-id (cdr (assoc :user-id comment))))
      (format out-stream "<li id=\"comment-~A\" class=\"comment-item~{ ~A~}\">"
	      c-id
	      (list-cond
	       (t (if (let ((is-odd (or (not level) (evenp level)))) ;inverted because level counts from 0
			(if level-invert (not is-odd) is-odd))
		      "depth-odd" "depth-even"))
	       ((and *current-ignore-hash* (gethash user-id *current-ignore-hash*)) "ignored")))
      (unwind-protect
        (comment-to-html out-stream comment :with-post-title with-post-title)
        (if extra-html-fn (funcall extra-html-fn c-id))
        (format out-stream "</li>")))))

(defun comment-tree-to-html (out-stream comment-hash &key (target nil) (level (if target 1 0)) level-invert)
  (let ((comments (gethash target comment-hash)))
    (when comments
      (comment-thread-to-html out-stream
        (lambda ()
          (loop for c in comments do
	       (comment-item-to-html out-stream c
		 :level level
		 :level-invert level-invert
                 :extra-html-fn (lambda (c-id)
				  (if (and (= level 10) (gethash c-id comment-hash))
				      (format out-stream "<input type=\"checkbox\" id=\"expand-~A\"><label for=\"expand-~:*~A\" data-child-count=\"~A comment~:P\">Expand this thread</label>"
					      c-id
					      (cdr (assoc :child-count c))))
				  (comment-tree-to-html out-stream comment-hash :target c-id :level (1+ level) :level-invert level-invert)))))))))

(defun sort-items (items sort-by)
  (multiple-value-bind (sort-fn key-fn)
      (ecase sort-by
	((:old :new) (values (if (eq sort-by :old)
				 (lambda (a b) (ignore-errors (local-time:timestamp< a b)))
				 (lambda (a b) (ignore-errors (local-time:timestamp> a b))))
			     (lambda (c) (ignore-errors (local-time:parse-timestring (or (cdr (assoc :posted-at c))
											 (cdr (assoc :created-at c)))))))))
    (sort items sort-fn :key key-fn)))

(defun comment-chrono-to-html (out-stream comments)
  (let ((comment-hash (make-comment-parent-hash comments)) 
	(comments (sort-items comments :old)))
    (comment-thread-to-html out-stream
      (lambda ()
        (loop for c in comments do
              (let* ((c-id (cdr (assoc :--id c)))
                     (new-c (acons :children (gethash c-id comment-hash) c)))
                (comment-item-to-html out-stream new-c)))))))
