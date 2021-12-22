(uiop:define-package #:lw2.colors
  (:use #:cl #:iterate #:lw2.utils)
  (:import-from #:alexandria #:when-let #:emptyp)
  (:export #:decode-css-color #:encode-css-color #:safe-color-name #:perceptual-invert-rgba #:perceptual-invert-color-string))

(in-package #:lw2.colors)

;;;; Refer to https://drafts.csswg.org/css-color/#numeric-srgb

(defparameter *web-colors-list*
  (with-open-file (stream (asdf:system-relative-pathname :lw2-viewer "data/webcolors.json") :direction :input)
    (map 'list
	 (lambda (color-data)
	   (cons (cdr (assoc :name color-data))
		 (map 'list (lambda (x) (/ (the (integer 0 255) (cdr x)) 255.0d0)) (cdr (assoc :rgb color-data)))))
	 (lw2.json:decode stream))))

(defun parse-multi-hex (string count length &key (start 0) (key #'identity))
  (values-list
   (iter (for i from start below (+ start (* count length)) by length)
	 (collect (funcall key (parse-integer string :start i :end (+ i length) :radix 16))))))

(defun parse-css-rgb-value (string)
  (let ((number (parse-integer string :junk-allowed t)))
    (if (ppcre:scan "%$" string)
	(/ number 100.0d0)
	(/ number 255.0d0))))

(defun parse-css-hue-value (string)
  (let ((number (arnesi:parse-float string :junk-allowed t)))
    (regex-case string
		("grad$" (* number (/ 360.0d0 400.0d0)))
		("rad$" (* number (/ 360.0d0 (* 2 pi))))
		("turn$" (* number 360.0d0))
		("%$" (* number (/ 360.0d0 100.0d0)))
		(t number))))

(defun parse-css-alpha-value (string)
  (let ((number (and (not (emptyp string)) (arnesi:parse-float string :junk-allowed t))))
    (if number
	(regex-case string
		    ("%$" (/ number 100d0))
		    (t number))
	1.0d0)))

(defun decode-css-color (color-string)
  (regex-case color-string
	      ("#[0-9a-fA-F]{3}\\s*$" (values* (parse-multi-hex color-string 3 1 :start 1 :key (lambda (x) (declare (type (integer 0 15) x)) (/ (+ x (* x 16)) 255.0d0))) 1.0d0))
	      ("#[0-9a-fA-F]{4}\\s*$" (parse-multi-hex color-string 4 1 :start 1 :key (lambda (x) (declare (type (integer 0 15) x)) (/ (+ x (* x 16)) 255.0d0))))
	      ("#[0-9a-fA-F]{6}\\s*$" (values* (parse-multi-hex color-string 3 2 :start 1 :key (lambda (x) (declare (type (integer 0 255) x)) (/ x 255.0d0))) 1.0d0))
	      ("#[0-9a-fA-F]{8}\\s*$" (parse-multi-hex color-string 4 2 :start 1 :key (lambda (x) (declare (type (integer 0 255) x)) (/ x 255.0d0))))
	      ("rgba?\\((.*?)\\)"
	       (multiple-value-bind (rgb-list a-list) (firstn (ppcre:split "[ ,]+" (reg 0)) 3)
		 (values* (values-list (map 'list #'parse-css-rgb-value rgb-list))
			  (parse-css-alpha-value (first a-list)))))
	      ("hsla?\\((.*?)\\)"
	       (destructuring-bind (h s l &optional a) (ppcre:split "[ ,/]+" (reg 0))
		 (values* (dufy/core:hsl-to-rgb (parse-css-hue-value h)
						(parse-css-rgb-value s)
						(parse-css-rgb-value l))
			  (parse-css-alpha-value a))))
	      (t
	       (when-let ((color-list (cdr (assoc color-string *web-colors-list* :test #'string-equal))))
		 (values* (values-list color-list) 1.0d0)))))

(defun encode-css-color (r g b a)
  (format nil "#~6,'0X~2,'0X" (dufy/core:rgb-to-rgbpack r g b) (round (* a 255))))

(defun safe-color-name (r g b a)
  (format nil "~6,'0X~2,'0X" (dufy/core:rgb-to-rgbpack r g b) (round (* a 255))))

(defun gamma-invert-lightness (l)
  (if (>= l 100d0)
      0d0
      (* 100d0 (expt (- 1d0 (/ l 100d0)) (/ 1.25d0)))))

(defun perceptual-invert-rgba (r g b alpha)
  (multiple-value-bind (l a b)
      (multiple-value-call #'dufy/core:xyz-to-lab (dufy/core:rgb-to-xyz r g b))
    (multiple-value-bind (nr ng nb)
	(multiple-value-call #'dufy/core:xyz-to-rgb
	  (dufy/core:lab-to-xyz (gamma-invert-lightness l) a b))
      (values nr ng nb alpha))))

(defun perceptual-invert-color-string (color-string)
  (multiple-value-call #'encode-css-color (multiple-value-call #'perceptual-invert-rgba (decode-css-color color-string))))
