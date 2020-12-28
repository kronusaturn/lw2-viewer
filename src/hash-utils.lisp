(uiop:define-package #:lw2.hash-utils
  (:use #:cl #:iter)
  (:import-from #:flexi-streams #:string-to-octets #:octets-to-string #:with-output-to-sequence)
  (:export #:city-hash-128-vector #:hash-string #:hash-printable-object #:hash-file-list)
  (:recycle #:lw2.lmdb))

(in-package #:lw2.hash-utils)

(defun city-hash-128-vector (data)
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (r1 r2) (city-hash:city-hash-128
				  (coerce data '(simple-array (unsigned-byte 8) (*))))
      (setf (nibbles:ub64ref/be array 0) r1
	    (nibbles:ub64ref/be array 8) r2))
    array))

(defun hash-string (string)
  (city-hash-128-vector (string-to-octets string :external-format :utf-8)))

(defun hash-printable-object (object)
  (hash-string (write-to-string object :circle nil :escape nil :pretty nil)))

(defun hash-file-list (file-list)
  (city-hash-128-vector
   (with-output-to-sequence (out-stream)
     (iter (for f in file-list)
	   (with-open-file (in-stream (asdf:system-relative-pathname :lw2-viewer f) :direction :input :element-type '(unsigned-byte 8))
	     (uiop:copy-stream-to-stream in-stream out-stream :element-type '(unsigned-byte 8)))))))
