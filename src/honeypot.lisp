(uiop:define-package #:lw2.honeypot
    (:use #:cl)
  (:export #:emit-random-sentence #:random-url))

(in-package #:lw2.honeypot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-word-array ()
    (labels ((iterate-words (fn)
	       (with-open-file (stream "/usr/share/dict/words" :direction :input)
		 (loop for word = (read-line stream nil nil)
		       while word
		       do (unless (find #\' word)
			    (funcall fn word))))))
      (let ((count 0))
	(iterate-words (lambda (word)
			 (declare (ignore word))
			 (incf count)))
	(let ((array (make-array count :element-type 'string :initial-element ""))
	      (i 0))
	  (iterate-words (lambda (word)
			   (setf (aref array i) word)
			   (incf i)))
	  array)))))

(sb-ext:defglobal *word-array* (read-word-array))

(defun capitalize (word)
  (let ((result (copy-seq word)))
    (setf (aref result 0) (char-upcase (aref result 0)))
    result))

(defun random-word (&optional capitalize)
  (let* ((word (aref *word-array* (random (length *word-array*)))))
    (if capitalize (capitalize word) word)))

(defun emit-random-sentence ()
    (loop for initial = t then nil
	  do (unless initial (write-char #\Space))
	  (write-string (random-word initial))
	  while (plusp (random 5))))

(defun random-url ()
  (with-output-to-string (*standard-output*)
    (write-string "/jorts/")
    (loop do (write-string (random-word))
	  (write-char #\/)
	  while (plusp (random 4)))))
