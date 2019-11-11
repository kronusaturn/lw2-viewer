(uiop:define-package #:lw2.clean-html
  (:use #:cl #:alexandria #:iterate #:split-sequence #:lw2.lmdb #:lw2.links #:lw2.utils #:lw2.context #:lw2.sites #:lw2.conditions)
  (:export #:*before-clean-hook* #:*link-hook* #:url-scanner #:clean-text #:clean-text-to-html #:clean-html #:clean-html* #:extract-excerpt #:extract-excerpt*)
  (:unintern #:*text-clean-regexps* #:*html-clean-regexps*))

(in-package #:lw2.clean-html)

(setf cl-typesetting-hyphen::*hyphen-patterns-directory* (asdf:system-relative-pathname "lw2-viewer" "data/hyphenation-patterns/"))
(setf cl-typesetting-hyphen::*language-hyphen-file-list* '((:en-us . "hyph_en_US")))
(cl-typesetting-hyphen:load-language :en-us)
(setf cl-typesetting::*default-hyphen-language* :en-us)

(defvar *before-clean-hook* nil)
(defvar *link-hook* nil)

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (uiop:slurp-stream-string stream)))

(defun grab-from-rts (url)
  (declare (optimize (speed 0) (space 3)))
  (let* ((root (plump:parse (drakma:http-request url :close t)))
	 (post-body (plump:get-element-by-id root "wikitext")))
    (loop for cls in '("div.nav_menu" "div.imgonly" "div.bottom_nav") do
	  (loop for e across (clss:select cls post-body)
		do (plump:remove-child e))) 
    (plump:remove-child (elt (clss:select "h1" post-body) 0))
    (plump:remove-child (elt (clss:select "p" post-body) 0))
    (with-open-file (stream (merge-pathnames "./rts-content/" (subseq (puri:uri-path (puri:parse-uri url)) 1)) :direction :output :if-does-not-exist :create :external-format :utf-8) 
		 (plump:serialize post-body stream))))

(defun rts-to-html (file)
  (declare (optimize (speed 0) (space 3)))
  (concatenate 'string
	       "<style>"
	       (file-get-contents "./rts-content/rts.css")
	       "</style>"
	       (file-get-contents (merge-pathnames "./rts-content/" file)))) 

(defparameter *html-overrides* (make-hash-table :test 'equal))
(loop for (id file) in '(("XTXWPQSEgoMkAupKt" "An-Intuitive-Explanation-Of-Bayess-Theorem") 
			 ("afmj8TKAqH6F2QMfZ" "A-Technical-Explanation-Of-Technical-Explanation")
			 ("7ZqGiPHTpiDMwqMN2" "The-Twelve-Virtues-Of-Rationality"))
      do (let ((file* file)) (setf (gethash id *html-overrides*) (lambda () (rts-to-html file*)))))

(defmacro do-with-cleaners ((regexp-list scanner replacement) &body body)
  (declare (optimize (speed 0) (space 3)))
  `(labels ((fn (,scanner ,replacement) ,@body))
     ,@(loop for (regex flags replacement) in (eval regexp-list)
             collecting `(fn (load-time-value
                               (ppcre:create-scanner ,regex
                                                     ,@(loop for (flag sym) in '((#\i :case-insensitive-mode)
                                                                                 (#\m :multi-line-mode)
                                                                                 (#\s :single-line-mode)
                                                                                 (#\x :extended-mode))
                                                             when (find flag flags)
                                                             append (list sym t))))
                             ,replacement))))

(defmacro define-cleaner (name regexp-list)
  (declare (optimize (speed 0) (space 3)))
  `(defun ,name (text)
     (do-with-cleaners (,regexp-list scanner replacement)
       (setf text (ppcre:regex-replace-all scanner text replacement)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-regexp-file (filename)
    (declare (optimize (speed 0) (space 3)))
    (let ((data (destructuring-bind (* ((* (* inner))))
		  (with-open-file (stream (uiop:subpathname (asdf:system-source-directory "lw2-viewer") filename)) (parse-js:parse-js stream))
		  inner)))
      (loop for input in data
	    collecting (destructuring-bind (* ((* regex flags) (* replacement))) input
			 (list regex flags (ppcre:regex-replace-all "\\$(\\d)" replacement "\\\\\\1")))))))

(define-cleaner clean-text (read-regexp-file "text-clean-regexps.js"))
(define-cleaner clean-html-regexps (read-regexp-file "html-clean-regexps.js"))

(declaim (ftype function url-scanner))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'url-scanner) (ppcre:create-scanner
				    "(?:https?://[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+|[-a-zA-Z0-9.]+\\.(?:com|edu|gov|mil|net|org|int|biz|info|name|museum|us|ca|uk|io|ly))(?:\\:[0-9]+){0,1}(?:/(?:(?:(\\()|[-\\w\\d.,;:?'\\\\+@!&%$#=~_/])*(?(1)\\)|[-\\w\\d\\\\+@&%$#=~_/]))?)?"
				    :single-line-mode t)))

(defun hyphenate-string (string)
 (let ((hyphenation-list (cl-typesetting::hyphenate-string string)))
   (declare (type (and string (not base-string)) string)
            (type list hyphenation-list))
   (if hyphenation-list
     (let ((new-string (make-array (+ (length string) (length hyphenation-list)) :element-type 'character :fill-pointer 0)))
       (loop for char across string
             for orig-offset of-type fixnum from 0
             with current-hyphenation = hyphenation-list
             do (when (and current-hyphenation (= orig-offset (the fixnum (first current-hyphenation))))
                  (vector-push #\SOFT_HYPHEN new-string)
                  (setf current-hyphenation (rest current-hyphenation)))
             do (vector-push char new-string))
       (values new-string hyphenation-list))
     (values string nil))))

(defun clean-text-to-html (text &key (hyphenation t))
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (clean-html-regexps
      (plump:encode-entities
        (coerce
          (funcall (if hyphenation #'hyphenate-string #'identity) (clean-text text))
          'simple-string)))))

(declaim (ftype (function (plump:node &rest simple-string) boolean) tag-is class-is-not text-class-is-not))

(defun tag-is (node &rest args)
  (declare (type plump:node node)
           (dynamic-extent args))
  (when (plump:element-p node)
    (let ((tag (plump:tag-name node)))
      (to-boolean
       (some (lambda (x) (string= tag x))
	     args)))))

(defun class-is-not (node &rest args)
  (declare (type plump:node node)
           (dynamic-extent args))
  (to-boolean
    (or
      (plump:root-p node)
      (and (not (intersection (split-sequence #\Space (or (plump:attribute node "class") "")) args :test #'string=))
           (or (null (plump:parent node)) (apply #'class-is-not (plump:parent node) args))))))

(defun text-class-is-not (node &rest args)
  (declare (type plump:node node)
           (dynamic-extent args))
  (apply #'class-is-not (plump:parent node) args))

(defun clean-dom-text (root)
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (labels
      ((recursep (node)
         (and (plump:element-p node)
              (ppcre:scan "^(?:p|div|blockquote|li|h[0-6])$" (plump:tag-name node))))
       (cleanablep (node)
         (and (plump:text-node-p node)
	      (plump:parent node)
              (text-class-is-not node "mjx-math" "arbital-math")))
       (traverse (node main-fn &optional recurse-fn)
         (when (cleanablep node) (funcall main-fn node))
         (when (plump:nesting-node-p node)
           (loop for n across (plump:children node)
                 do (if (recursep n)
                        (if recurse-fn (funcall recurse-fn n))
                        (traverse n main-fn recurse-fn))))))
      (let* ((offset-list nil)
             (whole-string-input
               (with-output-to-string (stream)
                 (traverse
                   root
                   (lambda (node)
                     (push (length (the string (plump:text node))) offset-list)
                     (write-string (plump:text node) stream))
                   #'clean-dom-text)))
             (whole-string-output whole-string-input))
        (declare (type string whole-string-output whole-string-input))
        (setf offset-list (nreverse offset-list))
        (labels
          ((call-with-offset-loop (continue-fn loop-fn next-offset-fn offset-adjust-fn)
             (loop with current-offset = offset-list
                   with output-offset of-type (or null fixnum) = (first current-offset)
                   with output-offset-list = nil
                   with total-offset of-type fixnum = 0
                   while (funcall continue-fn)
                   do (funcall loop-fn)
                   do (loop for current-offset-num of-type fixnum = (first current-offset)
                            while (and (rest current-offset) (< (+ total-offset current-offset-num) (funcall next-offset-fn)))
                            do (progn
                                 (push output-offset output-offset-list)
                                 (setf total-offset (+ total-offset current-offset-num)
                                       current-offset (cdr current-offset)
                                       output-offset (first current-offset))))
                   do (setf output-offset (funcall offset-adjust-fn output-offset))
                   finally (progn
                             (push output-offset output-offset-list)
                             (loop for x in (rest current-offset) do (push x output-offset-list))
                             (setf offset-list (nreverse output-offset-list))))
             (values)))
          (declare (dynamic-extent (function call-with-offset-loop))
                   (ftype (function ((function ()) (function ()) (function () fixnum) (function (fixnum) fixnum)) (values)) call-with-offset-loop))
          (macrolet
            ((offset-loop ((list-binding list-form) (&body loop-body) (&body next-offset-body) (&body offset-adjust-body))
               (with-gensyms (list-current)
                             `(let ((,list-current ,list-form)
                                    (,list-binding))
                                (labels ((continue-fn () (if ,list-current (setf ,list-binding (pop ,list-current))))
                                         (loop-fn () ,.loop-body)
                                         (next-offset-fn () ,.next-offset-body)
                                         (offset-adjust-fn ,(first offset-adjust-body) (declare (type fixnum ,(caar offset-adjust-body)) (values fixnum)) ,.(rest offset-adjust-body)))
                                  (declare (dynamic-extent (function continue-fn) (function loop-fn) (function next-offset-fn) (function offset-adjust-fn)))
                                  (call-with-offset-loop #'continue-fn #'loop-fn #'next-offset-fn #'offset-adjust-fn))))))
            (do-with-cleaners ((read-regexp-file "text-clean-regexps.js") scanner replacement)
              (let ((replacements 0)
                    (replacement-list nil)
                    (original-length (length whole-string-output)))
                (declare (type fixnum replacements))
                (ppcre:do-scans (match-start match-end reg-starts reg-ends scanner whole-string-output)
                                (declare (type fixnum match-start match-end)
                                         (type simple-vector reg-starts reg-ends))
                                (incf replacements)
                                (push
                                  (list (if (and (> (length reg-starts) 0) (eq (aref reg-starts 0) match-start))
                                            (aref reg-ends 0)
                                            match-start)
                                        (if (and (> (length reg-starts) 0) (eq (aref reg-ends (- (length reg-ends) 1)) match-end))
                                            (aref reg-starts (- (length reg-starts) 1))
                                            match-end))
                                  replacement-list))
                (setf replacement-list (nreverse replacement-list))
                (setf whole-string-output (ppcre:regex-replace-all scanner whole-string-output replacement))
                (let ((length-difference (- (length whole-string-output) original-length))
                      (length-change 0))
                  (declare (type fixnum length-difference length-change))
                  (offset-loop
                    (current-replacement replacement-list)
                    ((setf length-change (ceiling length-difference replacements)
                           length-difference (- length-difference length-change)
                           replacements (- replacements 1)))
                    ((destructuring-bind (start end) current-replacement
                       (declare (type fixnum start end))
                       (ceiling (+ start end) 2)))
                    ((output-offset) (max 0 (+ output-offset length-change)))))))
            (multiple-value-bind (hyphenated-string hyphenation-list) (hyphenate-string whole-string-output)
              (setf whole-string-output hyphenated-string)
              (offset-loop
                (current-hyphenation hyphenation-list)
                ()
                (current-hyphenation)
                ((output-offset) (1+ output-offset))))))
        (let ((current-offset 0))
          (declare (type (or null fixnum) current-offset))
          (traverse
            root
            (lambda (node)
              (let ((output-length (length whole-string-output))
                    (next-offset (if offset-list (+ current-offset (the fixnum (first offset-list))) nil)))
                (declare (type (or null fixnum) next-offset))
                (setf (plump:text node) (subseq whole-string-output (min current-offset output-length) (and next-offset (min next-offset output-length)))
                      current-offset next-offset
                      offset-list (cdr offset-list))))
            (lambda (node) (declare (ignore node))))))))
  root)

(define-lmdb-memoized extract-excerpt 'lw2.backend-modules:backend-lmdb-cache
  (:sources ("src/clean-html.lisp")) (in-html)
  (let ((root (plump:parse (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) in-html)))
	(chars 0)
	(need-space nil))
    (with-output-to-string (out-stream)
      (block nil
	(plump:traverse
	 root
	 (lambda (node)
	   (when (or (> (length (plump:children node)) 1)
		     (plump:text-node-p (plump:first-child node)))
	     (let ((text (plump:text node)))
	       (when need-space
		 (write-char #\Space out-stream))
	       (write-string text out-stream)
	       (setf chars (+ chars (length text))
		     need-space t)
	       (when (> chars 480)
		 (return nil)))))
	 :test (lambda (node) (tag-is node "p")))))))

(define-lmdb-memoized clean-html 'lw2.backend-modules:backend-lmdb-cache
  (:sources ("src/clean-html.lisp" "src/links.lisp" "text-clean-regexps.js" "html-clean-regexps.js")) (in-html &key with-toc post-id)
  (declare (ftype (function (plump:node) fixnum) plump:child-position)
           (ftype (function (plump:node) (and vector (not simple-array))) plump:family)
           (ftype (function (plump:node) simple-string) plump:text plump:tag-name))
  (labels ((only-child-is (node &rest args)
			  (declare (type plump:node node)
				   (dynamic-extent args)) 
			  (and (= 1 (length (plump:children node)))
			       (let ((child (plump:first-child node))) 
				 (and 
				   (typep child 'plump:element)
				   (apply #'tag-is (cons child args))))))
           (is-child-of-tag (node &rest args)
             (declare (type plump:node node)
                      (dynamic-extent args))
             (loop for e = (plump:parent node) then (plump:parent e)
                   while (not (typep e 'plump:root))
                   when (and (plump:element-p e) (apply #'tag-is (cons e args))) return t))
           (add-class (node class)
             (declare (type plump:node node)
                      (type string class))
             (let ((classes (adjoin class (alexandria:if-let (attr (plump:attribute node "class")) (split-sequence #\Space attr)) :test #'string=)))
               (declare (dynamic-extent classes))
               (setf (plump:attribute node "class") (format nil "~{~A~^ ~}" classes)))
             node)
           (make-element-before (node tag)
             (if (plump:text-node-p node)
                 (make-element-before (plump:parent node) tag)
                 (let ((e (plump:make-element (plump:parent node) tag)))
                   (plump:remove-child e)
                   (plump:insert-before node e)
		   (setf (plump:parent e) (plump:parent node))
		   e)))
           (wrap-children (node element-name)
             (let ((new-element (plump:make-element node element-name)))
               (plump:remove-child new-element)
               (setf (plump:children new-element) (plump:clone-children node t new-element)
                     (plump:children node) (plump:make-child-array))
               (plump:append-child node new-element)))
	   (move-children-out-of-node (node &key keep)
	     (iterate (for c in-vector (plump:children node) downto 0)
		      (setf (plump:parent c) (plump:parent node))
		      (plump:insert-after node c))
	     (if keep
		 (setf (plump:children node) (plump:make-child-array))
		 (plump:remove-child node)))
	   (text-node-is-not (node &rest args)
			     (declare (type plump:node node) 
				      (dynamic-extent args)) 
			     (or
			       (typep (plump:parent node) 'plump:root)
			       (every (lambda (x) (string/= (plump:tag-name (plump:parent node)) x)) args)))
	   (adjacent-text-node (node direction)
	     (multiple-value-bind (get-sibling insert-sibling)
		 (ecase direction
		   (:previous (values #'plump:previous-sibling #'plump:insert-before))
		   (:next (values #'plump:next-sibling #'plump:insert-after)))
	       (let ((candidate (funcall get-sibling node)))
		 (if (plump:text-node-p candidate)
		     candidate
		     (let ((new-node (plump:make-text-node (plump:parent node))))
		       (funcall insert-sibling node new-node)
		       new-node)))))
	   (char-is-whitespace (c)
	     (or (cl-unicode:has-binary-property c "White_Space")
		 (eql c #\BRAILLE_PATTERN_BLANK)))
	   (string-is-whitespace (string)
	     (every #'char-is-whitespace string))
	   (remove-if-whitespace (node)
	     (when (string-is-whitespace (plump:text node))
	       (plump:remove-child node)))
           (first-non-whitespace-child (node)
             (loop for e across (plump:children node)
		when (or (typep e 'plump:element) (not (string-is-whitespace (plump:text e)))) return e))
	   (find-text-node (node direction)
	     (let ((iterator #.`(case direction
				  ,@(loop for d in '(:first :last) collect
					 `(,d (lambda (fn)
						(iterate (for c in-vector (plump:children node) ,@(if (eq d :last) '(downto 0)))
							 (funcall fn c))))))))
	       (declare (dynamic-extent iterator))
	     (cond
	       ((and (plump:text-node-p node) (plump:parent node))
		node)
	       ((plump:nesting-node-p node)
		(block nil
		  (funcall iterator (lambda (c)
				      (when-let (tn (find-text-node c direction)) (return tn)))))))))
	   (vacuum-whitespace (node)
	     (dolist (direction '(:first :last))
	       (let ((displaced-text (make-string 0)))
		 (loop for lt = (find-text-node node direction) do
		      (cond
			((not lt)
			 (return-from vacuum-whitespace node))
			((string-is-whitespace (plump:text lt))
			 (setf displaced-text (case direction
						(:first (concatenate 'string displaced-text (plump:text lt)))
						(:last (concatenate 'string (plump:text lt) displaced-text))))
			 (plump:remove-child lt))
			(t
			 (let* ((text (plump:text lt)))
			   (case direction
			     (:first
			      (let ((boundary (loop for i from 0 to (- (length text) 1)
					     unless (char-is-whitespace (aref text i))
						 return i)))
				(setf displaced-text (concatenate 'string displaced-text (subseq text 0 boundary))
				      (plump:text lt) (subseq text boundary))))
			     (:last
			      (let ((boundary (loop for i from (- (length text) 1) downto 0
						 unless (char-is-whitespace (aref text i))
						 return i)))
				(setf displaced-text (concatenate 'string (subseq text (+ 1 boundary) (length text)))
				      (plump:text lt) (subseq text 0 (+ 1 boundary)))))))
			 (return))))
		 (when (> (length displaced-text) 0)
		   (let ((atn (adjacent-text-node node (case direction (:first :previous) (:last :next)))))
		     (setf (plump:text atn) (case direction
					      (:first (concatenate 'string (plump:text atn) displaced-text))
					      (:last (concatenate 'string displaced-text (plump:text atn))))))))))
	   (add-element-style (node attribute value)
	     (let ((old-style (plump:attribute node "style")))
	       (setf (plump:attribute node "style")
		     (if old-style
			 (format nil "~A~:[;~;~] ~A: ~A;" old-style (ppcre:scan ";\s*$" old-style) attribute value)
			 (format nil "~A: ~A;" attribute value)))))
	   (flatten-element (node)
	     (let* ((previous-sibling (plump:previous-sibling node))
		    (next-sibling (if (plump:text-node-p (plump:next-sibling node))
				      (plump:next-sibling node)))
		    (new-text-node (if (plump:text-node-p previous-sibling)
				       previous-sibling
				       (plump:insert-before node
							    (plump:remove-child (plump:make-text-node (plump:parent node)))))))
	       (setf (plump:parent new-text-node) (plump:parent node)
		     (plump:text new-text-node) (concatenate 'string
							     (plump:text new-text-node)
							     (plump:text node)
							     (if next-sibling
								 (plump:text next-sibling)
								 "")))
	       (plump:remove-child node)
	       (when next-sibling (plump:remove-child next-sibling))))
	   (scan-for-urls (text-node)
			  (declare (type plump:text-node text-node)) 
			  (let ((text (plump:text text-node)))
			    (multiple-value-bind (url-start url-end)
				(ppcre:scan #'url-scanner text)
			      (declare (type simple-string text)
                                       (type (or null fixnum) url-start url-end))
			      (when url-start
				(let* ((url-raw (subseq text url-start url-end))
				       (url (if (mismatch "http" url-raw :end2 4) (concatenate 'string "http://" url-raw) url-raw)) 
				       (family (plump:family text-node)) 
				       (other-children (prog1
							 (subseq family (1+ (plump:child-position text-node)))
							 (setf (fill-pointer family) (1+ (plump:child-position text-node))))) 
				       (new-a (plump:make-element (plump:parent text-node) "a"))
				       (new-text (unless (= url-end (length text)) (plump:make-text-node (plump:parent text-node) (subseq text url-end))))) 
				  (setf (plump:text text-node) (subseq text 0 url-start)
                                        (plump:attribute new-a "href") (with-direct-link (convert-any-link url))
					(plump:attribute new-a "class") "bare-url")
				  (plump:make-text-node new-a (clean-text url-raw))
				  (when new-text
				    (scan-for-urls new-text)
				    (setf (plump:text new-text) (clean-text (plump:text new-text))))
				  (loop for item across other-children
				     do (plump:append-child (plump:parent text-node) item))
				  (when (= url-start 0)
				    (plump:remove-child text-node)))))))
	   (title-to-anchor (text used-anchors)
	     ;; This should match LW behavior in packages/lesswrong/lib/collections/posts/tableOfContents.js
	     (let* ((chars-to-use "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
		    (base-anchor (with-output-to-string (stream)
				   (loop for c across text
				      do (write-char (if (find c chars-to-use) c #\_) stream)))))
		   (loop for suffix from 0
		      for anchor = base-anchor then (format nil "~A~A" base-anchor suffix)
		      when (not (gethash anchor used-anchors))
		      return (progn (setf (gethash anchor used-anchors) t)
				    anchor))))
	   (contents-to-html (contents min-header-level out-stream)
			     (declare (type cons contents)) 
			     (format out-stream "<nav class=\"contents\"><div class=\"contents-head\">Contents</div><ul class=\"contents-list\">")
			     (loop for (elem-level text id) in contents do
				  (format out-stream "<li class=\"toc-item-~A\"><a href=\"#~A\">~A</a></li>"
					  (- elem-level (- min-header-level 1)) id (clean-text-to-html text)))
			     (format out-stream "</ul></nav>"))
	   (style-hash-to-html (style-hash out-stream)
			       (declare (type hash-table style-hash))
			       (let ((style-list (alexandria:hash-table-keys style-hash)))
				 (if style-list
				   (format out-stream "<style>~{~A~}</style>" style-list)))))
    (declare (ftype (function (plump:node &rest simple-string) boolean) only-child-is is-child-of-tag text-node-is-not))
    (handler-bind
      (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
      (alexandria:if-let
	(override (gethash post-id *html-overrides*))
	(funcall override) 
	(let ((root (plump:parse (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) in-html)))
	      (contents nil)
	      (section-count 0)
	      (min-header-level 6) 
	      (aggressive-deformat nil)
	      (style-hash (make-hash-table :test 'equal))
	      (used-anchors (make-hash-table :test 'equal)))
          (declare (type fixnum section-count min-header-level))
	  (when *before-clean-hook*
	    (funcall *before-clean-hook*))
          (let ((wayward-li-container nil))
            (plump:traverse
	     root
	     (lambda (node)
	       (cond
		 ((not (plump:parent node)) nil)
		 ((tag-is node "a")
		  (cond
		    ((not (plump:attribute node "href"))
		     (move-children-out-of-node node :keep t))
		    ((and (ppcre:scan "https?://" (plump:text node))
			  (not (find #\HORIZONTAL_ELLIPSIS (plump:text node))))
		     (flatten-element node))
		    (t (tagbody start
			  (let* ((next-sibling (plump:next-sibling node))
				 (next-text-node (if (plump:text-node-p next-sibling) next-sibling))
				 (next-next-sibling (if next-text-node (plump:next-sibling next-text-node) next-sibling))
				 (next-a (if (and next-next-sibling (tag-is next-next-sibling "a")) next-next-sibling)))
			    (when (and next-a
				       (or (not next-text-node) (string-is-whitespace (plump:text next-text-node)))
				       (string= (plump:attribute node "href") (plump:attribute next-a "href")))
			      (when next-text-node
				(plump:remove-child next-text-node)
				(plump:append-child node next-text-node))
			      (loop for c across (plump:children next-a)
				 do (progn (plump:remove-child c)
					   (plump:append-child node c)))
			      (plump:remove-child next-a)
			      (go start)))))))
		 ((tag-is node "ul" "ol")
		  (setf wayward-li-container node)
		  (let ((new-children (plump:make-child-array)))
		    (loop for child across (plump:children node)
		       do (if (and (plump:element-p child) (tag-is child "li"))
			      (vector-push-extend child new-children)
			      (unless (and (plump:text-node-p child) (string-is-whitespace (plump:text child)))
				(if (= (length new-children) 0)
				    (vector-push-extend (plump:make-element node "li") new-children))
				(plump:append-child (aref new-children (- (length new-children) 1)) child))))
		    (setf (plump:children node) new-children)))
		 ((tag-is node "li")
		  (unless (is-child-of-tag node "ul" "ol")
		    (unless wayward-li-container
		      (setf wayward-li-container (make-element-before node "ul")))
		    (plump:remove-child node)
		    (plump:append-child wayward-li-container node)))
		 ((tag-is node "p" "blockquote" "div")
		  (setf wayward-li-container nil))))
	     :test #'plump:element-p))
	  (loop while (and (= 1 (length (plump:children root))) (plump:element-p (plump:first-child root)) (tag-is (plump:first-child root) "div" "html" "body"))
	     do (setf (plump:children root) (plump:children (plump:first-child root)))
	     do (loop for c across (plump:children root) do (setf (plump:parent c) root))
	     do (when-let (fc (plump:first-child root))
		  (when (and (plump:element-p fc) (tag-is fc "head"))
		    (loop for c across (plump:children fc) do
			 (when (and (plump:element-p c) (tag-is c "style"))
			   (setf (plump:parent c) (plump:parent fc))
			   (plump:insert-after fc c)))
		    (plump:remove-child fc))))
	  (loop for c across (plump:children root) do
	       (when (and (plump:element-p c)
			  (tag-is c "span")
			  (string-is-whitespace (plump:text c)))
		 (move-children-out-of-node c)))
	  (loop for lc = (plump:last-child root)
	     while (and (plump:element-p lc) (tag-is lc "br"))
	     do (plump:remove-child lc))
	  (plump:traverse
	   root
	   (lambda (node)
	     (when (and (plump:text-node-p node)
			(plump:parent node)
			(text-node-is-not node "a" "style" "pre"))
	       (scan-for-urls node))))
	  (plump:traverse
	   root
	   (lambda (node)
	     (when (and (not (plump:root-p node)) (plump:parent node))
	       (typecase node
		 (plump:text-node 
		  (when (and (text-node-is-not node "style" "pre" "code")
			     (text-class-is-not node "mjx-math"))
		    (let ((new-root (plump:parse (clean-html-regexps (plump:serialize node nil))))
			  (other-children (prog1
					      (subseq (plump:family node) (1+ (plump:child-position node)))
					    (setf (fill-pointer (plump:family node)) (plump:child-position node)))))
		      (loop for item across (plump:children new-root)
			 do (plump:append-child (plump:parent node) item))
		      (loop for item across other-children
			 do (plump:append-child (plump:parent node) item)))))
		 (plump:element
		  (alexandria:when-let (style (plump:attribute node "style"))
				       (cond ((or aggressive-deformat (search "font-family" style) (search "font-style: inherit" style)
						  (search "MsoNormal" (plump:attribute node "class")))
					      (setf aggressive-deformat t)
					      (plump:remove-attribute node "style"))
					     ((ppcre:scan "(?:^|;)\\s*(?:color:\\s*\\#000000|line-height:[^;]+in)\\s*(?:;|$)" style)
					      (plump:remove-attribute node "style"))))
		  (when (and aggressive-deformat (tag-is node "div"))
		    (setf (plump:tag-name node) "p"))
		  (when (search "mjx-math" (plump:attribute node "class"))
		    (loop for current = node then (plump:parent current)
		       for parent = (plump:parent current)
		       when (loop for s across (plump:family current)
			       unless (or (eq s current)
					  (and (plump:text-node-p s) (string-is-whitespace (plump:text s))))
			       return t)
		       do (progn (add-class current "mathjax-inline-container")
				 (return))
		       when (or (plump:root-p parent)
				(tag-is parent "p" "blockquote" "div"))
		       do (progn (add-class current "mathjax-block-container")
				 (return))))
		  (cond
		    ((tag-is node "a")
		     (vacuum-whitespace node)
		     (let ((href (plump:attribute node "href")))
		       (when href
			 (let* ((href (if (ppcre:scan "^(?:(?:[a-z]+:)?//|/|#)" href) href (format nil "http://~A" href)))
				(href (or (with-direct-link (convert-any-link href)) href)))
			   (when href
			     (setf (plump:attribute node "href") href)
			     (when *link-hook*
			       (log-and-ignore-errors
				(funcall *link-hook* href))))))))
		    ((tag-is node "img")
		     (when
			 (every (lambda (a) (if-let (attr (plump:attribute node a)) (ignore-errors (<= (parse-integer attr) 1)))) (list "width" "height"))
		       (plump:remove-child node))
		     (when (typep *current-site* 'alternate-frontend-site)
		       (let ((src (plump:attribute node "src")))
			 (when
			     (and (> (length src) 0) (string= "/" src :end2 1))
			   (setf (plump:attribute node "src") (quri:render-uri
							       (quri:merge-uris src (main-site-uri *current-site*))))))))
		    ((tag-is node "p" "blockquote" "div")
		     (if (string-is-whitespace (plump:text node))
			 (if (plump:get-elements-by-tag-name node "img")
			     (add-class node "imgonly")
			     (plump:remove-child node))
			 (if-let (parent (plump:parent node))
				 (labels ((spoilerp (n)
					    (if-let (a (and (plump:element-p n) (plump:attribute n "class")))
						    (ppcre:scan "(?:^| )spoiler(?: |$)" a))))
				   (cond
				     ((and (tag-is node "p")
					   (spoilerp node)
					   (spoilerp parent))
				      (plump:remove-attribute node "class"))
				     ((and (spoilerp node)
					   (tag-is node "p")
					   (not (spoilerp parent)))
				      (let ((new-container (plump:make-element parent "div")))
					(setf (plump:attribute new-container "class") "spoiler")
					(plump:remove-child new-container)
					(setf (plump:parent new-container) (plump:parent node))
					(plump:insert-before node new-container)
					(loop for e = node then ns
					   while (and (plump:element-p e) (spoilerp e))
					   for ns = (plump:next-sibling e)
					   do (progn
						(plump:remove-attribute e "class")
						(plump:remove-child e)
						(plump:append-child new-container e))))))))))
		    ((tag-is node "u")
		     (let ((parent (plump:parent node)))
		       (cond
			 ((and (or (plump:root-p parent) (and (plump:element-p parent) (tag-is parent "p" "blockquote" "div")))
			       (loop for c across (plump:children node) never (and (plump:element-p c) (tag-is c "a"))))
			  (vacuum-whitespace node))
			 (t
			  (move-children-out-of-node node)))))
		    ((tag-is node "ol")
		     (when-let (old-style (plump:attribute node "style"))
		       (setf (plump:attribute node "style")
			     (ppcre:regex-replace-all
			      (load-time-value (ppcre:create-scanner "list-style-type\\s*:\\s*decimal\\s*;?" :single-line-mode t :case-insensitive-mode t))
			      old-style
			      "")))
		     (when-let (start-string (plump:attribute node "start"))
		       (when-let (start (ignore-errors (parse-integer start-string)))
			 (plump:remove-attribute node "start")
			 (add-element-style node "counter-reset" (format nil "ol ~A" (- start 1))))))
		    ((tag-is node "li")
		     (when (let ((c (plump:first-child node))) (and c (if (plump:text-node-p c) (not (string-is-whitespace (plump:text c))) (not (tag-is c "p" "ul" "ol")))))
		       (wrap-children node "p")))
		    ((tag-is node "pre")
		     (let ((nchildren (length (plump:children node))))
		       (when (>= nchildren 1)
			 (remove-if-whitespace (plump:first-child node))
			 (when (>= nchildren 2)
			   (remove-if-whitespace (plump:last-child node))))))
		    ((ppcre:scan "^h[1-6]$" (plump:tag-name node))
		     (cond
		       ((string-is-whitespace (plump:text node))
			(plump:remove-child node))
		       ((plump:get-elements-by-tag-name node "p")
			(move-children-out-of-node node)))
		     (let ((fc (plump:first-child node))
			   (lc (plump:last-child node)))
		       (when (and (plump:element-p fc) (tag-is fc "br")) (plump:remove-child fc))
		       (when (and (not (eql fc lc)) (plump:element-p lc) (tag-is lc "br")) (plump:remove-child lc)))
		     (when with-toc
		       (incf section-count) 
		       (unless (plump:attribute node "id") (setf (plump:attribute node "id") (format nil "section-~A" section-count)))
		       (let* ((header-level (parse-integer (subseq (plump:tag-name node) 1)))
			      (header-text (with-output-to-string (stream)
					     (plump:traverse node
							     (lambda (n)
							       (typecase n
								 (plump:text-node
								  (when (text-node-is-not n "style" "script")
								    (write-string (plump:text n) stream))))))))
			      (anchor-old (or (plump:attribute node "id") (format nil "section-~A" section-count)))
			      (anchor-new (title-to-anchor header-text used-anchors))
			      (wrapper (wrap-children node "span")))
			 (setf min-header-level (min min-header-level header-level)
			       (plump:attribute node "id") anchor-new
			       (plump:attribute wrapper "id") anchor-old)
			 (push (list header-level
				     header-text
				     anchor-new)
			       contents))))
		    ((tag-is node "style")
		     (let ((text (plump:text node)))
		       (when (search ".mjx-math" text)
			 (setf (gethash text style-hash) t)))
		     (plump:remove-child node))
		    ((tag-is node "script")
		     (plump:remove-child node))))))))
	  (clean-dom-text root)
	  (let ((with-toc (>= section-count 3)))
	    (with-output-to-string (out-stream)
	      (style-hash-to-html style-hash out-stream)
	      (loop for c across (plump:children root)
		 when (and with-toc
			   (not (string-is-whitespace (plump:text c))))
		 do (progn
		      (contents-to-html (nreverse contents) min-header-level out-stream)
		      (setf with-toc nil))
		 do (plump:serialize c out-stream)))))))))
