(uiop:define-package #:lw2.raw-memory-streams
  (:use :cl :trivial-gray-streams)
  (:export #:raw-memory-stream))

(in-package #:lw2.raw-memory-streams)

(defclass raw-memory-stream (fundamental-binary-input-stream)
  ((pointer :initarg :pointer)
   (length :initarg :length)
   (position :initform 0 :accessor stream-file-position)))

(defmethod stream-element-type ((self raw-memory-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((self raw-memory-stream))
  (declare (optimize (safety 0) (debug 0)))
  (with-slots (pointer length position) self
    (if (>= position length)
	:eof
	(prog1
	    (cffi:mem-aref pointer :unsigned-char position)
	  (incf position)))))

(defmethod stream-read-sequence ((self raw-memory-stream) sequence start end &key)
  (declare (optimize (safety 0) (debug 0)))
  (with-slots (pointer length position) self
    (let* ((remaining-length (- length position))
	   (requested-length (- end start))
	   (actual-length (min remaining-length requested-length)))
      (macrolet ((inner ()
		   `(loop for mem-index from position to (+ position actual-length)
		       for sequence-index from start
		       do (setf (elt sequence sequence-index)
				(cffi:mem-aref pointer :unsigned-char mem-index)))))
	  (etypecase sequence
	    ((simple-array (unsigned-byte 8) (*)) (inner))
	    ((array (unsigned-byte 8) (*)) (inner))
	    (sequence (inner))))
      (incf position actual-length)
      (+ start actual-length))))
