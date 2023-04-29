(uiop:define-package #:lw2.colors
  (:use #:cl #:iterate #:lw2.utils)
  (:import-from #:alexandria #:when-let #:emptyp)
  (:import-from #:parse-float #:parse-float)
  (:export #:-css-color-scanner- #:decode-css-color #:encode-css-color #:safe-color-name #:perceptual-invert-rgba #:perceptual-invert-color-string))

(in-package #:lw2.colors)

;;;; Refer to https://drafts.csswg.org/css-color/#numeric-srgb

(global-vars:define-global-parameter -css-color-scanner- (ppcre:create-scanner "#[0-9a-fA-F]{3,8}|rgba?\\((?:.*?)\\)|hsla?\\((?:.*?)\\)"))

(defparameter *web-colors-list*
  (with-open-file (stream (asdf:system-relative-pathname :lw2-viewer "data/webcolors.json") :direction :input)
    (map 'list
	 (lambda (color-data)
	   (cons (cdr (assoc :name color-data))
		 (map 'list (lambda (x) (/ (the (integer 0 255) (cdr x)) 255.0d0)) (cdr (assoc :rgb color-data)))))
	 (json:decode-json stream))))

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
  (let ((number (parse-float string :junk-allowed t)))
    (regex-case string
		("grad$" (* number (/ 360.0d0 400.0d0)))
		("rad$" (* number (/ 360.0d0 (* 2 pi))))
		("turn$" (* number 360.0d0))
		("%$" (* number (/ 360.0d0 100.0d0)))
		(t number))))

(defun parse-css-alpha-value (string)
  (let ((number (and (not (emptyp string)) (parse-float string :junk-allowed t))))
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

(defun gamma-invert-lightness (l &optional gamma)
  (let ((gamma (or gamma 2.2d0)))
    (if (>= l 1d0)
	0d0
	(expt (- 1d0 l) (/ gamma)))))

(defun linear-to-srgb (r g b)
  (declare (optimize (debug 0))
	   (double-float r g b))
  (flet ((f (x)
	   (if (>= x 0.0031308)
	       (- (* 1.055 (the double-float (expt x (/ 2.4d0)))) 0.055)
	       (* 12.92 x))))
    (declare (inline f))
    (values (f r) (f g) (f b))))

(defun srgb-to-linear (r g b)
  (declare (optimize (debug 0))
	   (double-float r g b))
  (flet ((f (x)
	   (if (>= x 0.04045)
	       (the double-float (expt (/ (+ x 0.055) 1.055) 2.4d0))
	       (/ x 12.92))))
    (declare (inline f))
    (values (f r) (f g) (f b))))

(defun linear-srgb-to-oklab (r g b)
  (declare (optimize (debug 0))
	   (double-float r g b))
  (flet ((cbrt (x) (the double-float (expt x (/ 3.0d0)))))
    (declare (inline cbrt))
    (let ((l (cbrt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b))))
	  (m (cbrt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b))))
	  (s (cbrt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))))
      (values (+ (* 0.2104542553 l) (* +0.7936177850 m) (* -0.0040720468 s))
	      (+ (* 1.9779984951 l) (* -2.4285922050 m) (* +0.4505937099 s))
	      (+ (* 0.0259040371 l) (* +0.7827717662 m) (* -0.8086757660 s))))))

(defun ab-to-ch (a b)
  (values (sqrt (+ (expt a 2) (expt b 2)))
	  (atan b a)))

(defun ch-to-ab (c h)
  (values (* c (cos h))
	  (* c (sin h))))

(defun oklab-to-linear-srgb (l a b)
  (declare (optimize (debug 0))
	   (double-float l a b))
  (flet ((cube (x) (* x x x)))
    (declare (inline cube))
    (let ((l (cube (+ l (* 0.3963377774 a) (* 0.2158037573 b))))
	  (m (cube (- l (* 0.1055613458 a) (* 0.0638541728 b))))
	  (s (cube (- l (* 0.0894841775 a) (* 1.2914855480 b)))))
      (values (+ (* +4.0767416621 l) (* -3.3077115913 m) (* +0.2309699292 s))
	      (+ (* -1.2684380046 l) (* +2.6097574011 m) (* -0.3413193965 s))
	      (+ (* -0.0041960863 l) (* -0.7034186147 m) (* +1.7076147010 s))))))

(defun oklab-to-srgb (l a b)
  (declare (optimize (debug 0))
	   (double-float l a b))
  (flet ((in-gamut (l a b)
	   (multiple-value-bind (r g b) (multiple-value-call #'linear-to-srgb (oklab-to-linear-srgb l a b))
	     (and (not (> (max r g b) 1.0d0))
		  (not (< (min r g b) 0.0d0))))))
    (if (in-gamut l a b)
	(multiple-value-call #'linear-to-srgb (oklab-to-linear-srgb l a b))
	(let ((array (cl-grnm:grnm-optimize
		      (lambda (array)
			(let ((c-l (aref array 0))
			      (c-a (aref array 1))
			      (c-b (aref array 2)))
			  (declare (double-float c-l c-a c-b))
			  (if (in-gamut c-l c-a c-b)
			      (+ (* 10.0d0 (the double-float (expt (- l c-l) 2)))
				 (expt (- a c-a) 2)
				 (expt (- b c-b) 2))
			      most-positive-double-float)))
		      (vector (* (+ 0.05d0 (max 0.0d0 (min 1.0d0 l))) 0.9d0) 0.0d0 0.0d0)
		      :max-function-calls 10000)))
	  (let ((l (aref array 0))
		(a (aref array 1))
		(b (aref array 2)))
	    (multiple-value-call #'linear-to-srgb (oklab-to-linear-srgb l a b)))))))

(defun perceptual-invert-rgba (r g b alpha &optional gamma)
  (multiple-value-bind (l a b)
      (multiple-value-call #'linear-srgb-to-oklab (srgb-to-linear r g b))
    (multiple-value-bind (c h)
	(ab-to-ch a b)
      (multiple-value-bind (a b)
	  (ch-to-ab c (if (< -1.5591128900152316d0 h 2.372773855360125d0)
			  h
			  (+ (* (mod (- h 2.372773855360125d0) (* 2 pi))
				0.11248729401633725d0)
			     2.372773855360125d0)))
	(multiple-value-call #'values
	  (oklab-to-srgb (gamma-invert-lightness l gamma) a b)
	  alpha)))))

(defun perceptual-invert-color-string (color-string &optional gamma)
  (multiple-value-call #'encode-css-color (multiple-value-call #'perceptual-invert-rgba (decode-css-color color-string) gamma)))

(defun rewrite-css-colors (in-stream out-stream fn)
  (flet ((replacer (target-string start end match-start match-end reg-starts reg-ends)
	   (declare (ignore start end reg-starts reg-ends))
	   (funcall fn (substring target-string match-start match-end))))
    (declare (dynamic-extent #'replacer))
    (loop for in-line = (read-line in-stream nil)
       while in-line
       do (let ((out-line (ppcre:regex-replace-all -css-color-scanner- in-line #'replacer)))
	    (write-line out-line out-stream)))))
