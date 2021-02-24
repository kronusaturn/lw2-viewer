(uiop:define-package #:lw2.elicit-predictions
  (:use #:cl #:iterate #:lw2.utils #:lw2.html-reader #:lw2.backend #:lw2.graphql))

(in-package #:lw2.elicit-predictions)

(named-readtables:in-readtable html-reader)

(declaim (inline normal-pdf))
(defun normal-pdf (x u o)
  (* (/ 1 (sqrt (* 2 pi o))) (exp (/ (- (expt (- x u) 2)) (* 2 o)))))

(defun render-elicit-block (question-id)
  (let ((elicit-data
	 (lw2-graphql-query (graphql-query-string "ElicitBlockData" (alist :question-id question-id) '(:title :notes :resolves-by :resolution (:predictions :prediction (:creator (:lw-user :display-name)))))
			    :decoder (lambda (x) (cdadr (assoc :data (lw2.backend::deserialize-query-result x)))))))
    (alist-bind (title notes resolves-by resolution predictions) elicit-data
		<figure class="prediction-poll">
		<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewbox="0 0 700 115">
		(let* ((width 700)
		       (height 100)
		       (prediction-count (length predictions))
		       (bandwidth-scale (/ (float (* 3 width)) (sqrt prediction-count)))
		       (histogram (make-array 99 :element-type 'fixnum :initial-element 0))
		       (density (make-array (1+ (* 98 (/ width 100))) :element-type 'single-float :initial-element 0f0))
		       (max-bin 0)
		       (max-density 0f0))
		  (with-html-stream-output (:stream stream)
		    (iter (for prediction-data in predictions)
			  (alist-bind (prediction) prediction-data
				      (when (and prediction (< 0 prediction 100))
					(let ((n (incf (aref histogram (1- prediction)))))
					  (when (> n max-bin) (setf max-bin n))))))
		    (let* ((hist-scale (/ (float height) (1+ max-bin))))
		      (iter (for bin from 1 to 99)
			    (let ((hval (aref histogram (1- bin))))
			      (when (> hval 0)
				(format stream "<rect x=~D y=~5F width=4 height=~5F fill='currentColor' opacity='0.2' />"
					(- (* (/ width 100) bin) 2)
					(- height (* hist-scale hval))
					(* hist-scale hval)))))
		      (write-string "<path fill='none' stroke='currentColor' stroke-width='0.6667px' d='M " stream)
		      (iter (for x from (/ width 100) to (* 99 (/ width 100)))
			    (let ((y
				   (iter (for prediction-data in predictions)
					 (alist-bind (prediction) prediction-data
						     (sum (coerce (normal-pdf (float x) (* prediction (float (/ width 100))) bandwidth-scale) 'single-float))))))
			      (setf (aref density (- x (/ width 100))) y)
			      (when (> y max-density) (setf max-density y))))
		      (let ((height-scale (/ (1- height) (max (* (1+ max-bin) (normal-pdf 0f0 0f0 bandwidth-scale))
							      max-density))))
			(iter (for x from (/ width 100) to (* 99 (/ width 100)))
			      (let ((y (aref density (- x (/ width 100)))))
				(format stream "~D,~5F " x (- height (* y height-scale))))))
		      (write-string "' />" stream)
		      (iter (for x from 1 to 9)
			    (format stream "<text x=~5F y=~D font-size='12px' fill='currentColor'>~D%</text>" (* x (/ width 10)) (+ height 14) (* x 10))))))
		</svg>
		(when (nonempty-string title) <figcaption>(progn title)</figcaption>)
		</figure>)))
