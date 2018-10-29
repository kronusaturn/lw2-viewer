(uiop:define-package #:lw2.clean-html
  (:use #:cl #:alexandria #:split-sequence #:lw2.lmdb #:lw2.links)
  (:export #:clean-text #:clean-text-to-html #:clean-html #:clean-html*)
  (:unintern #:*text-clean-regexps* #:*html-clean-regexps*))

(in-package #:lw2.clean-html)

(eval-when (:load-toplevel :execute)
  (cl-typesetting-hyphen:load-language :british)
  (setf cl-typesetting::*default-hyphen-language* :british))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (uiop:slurp-stream-string stream)))

(defun grab-from-rts (url)
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
  `(defun ,name (text)
     (do-with-cleaners (,regexp-list scanner replacement)
       (setf text (ppcre:regex-replace-all scanner text replacement)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-regexp-file (filename)
    (let ((data (destructuring-bind (* ((* (* inner))))
		  (with-open-file (stream (uiop:subpathname (asdf:system-source-directory "lw2-viewer") filename)) (parse-js:parse-js stream))
		  inner)))
      (loop for input in data
	    collecting (destructuring-bind (* ((* regex flags) (* replacement))) input
			 (list regex flags (ppcre:regex-replace-all "\\$(\\d)" replacement "\\\\\\1")))))))

(define-cleaner clean-text (read-regexp-file "text-clean-regexps.js"))
(define-cleaner clean-html-regexps (read-regexp-file "html-clean-regexps.js"))

(defun hyphenate-string (string)
 (let ((hyphenation-list (cl-typesetting::hyphenate-string string)))
   (if hyphenation-list
     (let ((new-string (make-array (+ (length string) (length hyphenation-list)) :element-type 'character :fill-pointer 0)))
       (loop for char across string
             for orig-offset from 0
             with current-hyphenation = hyphenation-list
             do (when (and current-hyphenation (= orig-offset (first current-hyphenation)))
                  (vector-push #\SOFT_HYPHEN new-string)
                  (setf current-hyphenation (rest current-hyphenation)))
             do (vector-push char new-string))
       (values new-string hyphenation-list))
     (values string nil))))

(defun clean-text-to-html (text)
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (clean-html-regexps (plump:encode-entities (coerce (hyphenate-string (clean-text text)) 'simple-string)))))

(defun clean-dom-text (root)
  (handler-bind
    (((or plump:invalid-xml-character plump:discouraged-xml-character) #'abort))
    (let* ((offset-list nil)
           (whole-string-input
             (with-output-to-string (stream)
               (plump:traverse
                 root
                 (lambda (node)
                   (typecase node
                     (plump:text-node
                       (push (length (plump:text node)) offset-list)
                       (write-string (plump:text node) stream)))))))
           (whole-string-output whole-string-input))
      (setf offset-list (nreverse offset-list))
      (labels
        ((call-with-offset-loop (continue-fn loop-fn next-offset-fn offset-adjust-fn)
           (loop with current-offset = offset-list
                 with output-offset = (first current-offset)
                 with output-offset-list = nil
                 with total-offset = 0
                 while (funcall continue-fn)
                 do (funcall loop-fn)
                 do (loop while (and (rest current-offset) (< (+ total-offset (first current-offset)) (funcall next-offset-fn)))
                          do (progn
                               (push output-offset output-offset-list)
                               (setf total-offset (+ total-offset (first current-offset))
                                     current-offset (cdr current-offset)
                                     output-offset (first current-offset))))
                 do (setf output-offset (funcall offset-adjust-fn output-offset))
                 finally (progn
                           (push output-offset output-offset-list)
                           (loop for x in (rest current-offset) do (push x output-offset-list))
                           (setf offset-list (nreverse output-offset-list))))))
        (declare (dynamic-extent (function call-with-offset-loop)))
        (macrolet
          ((offset-loop ((list-binding list-form) (&body loop-body) (&body next-offset-body) (&body offset-adjust-body))
             (with-gensyms (list-current)
               `(let ((,list-current ,list-form)
                      (,list-binding))
                  (labels ((continue-fn () (if ,list-current (setf ,list-binding (pop ,list-current))))
                           (loop-fn () ,.loop-body)
                           (next-offset-fn () ,.next-offset-body)
                           (offset-adjust-fn ,.offset-adjust-body))
                    (declare (dynamic-extent (function continue-fn) (function loop-fn) (function next-offset-fn) (function offset-adjust-fn)))
                    (call-with-offset-loop #'continue-fn #'loop-fn #'next-offset-fn #'offset-adjust-fn))))))
          (do-with-cleaners ((read-regexp-file "text-clean-regexps.js") scanner replacement)
            (let ((replacements 0)
                  (replacement-list nil)
                  (original-length (length whole-string-output)))
              (ppcre:do-scans (match-start match-end reg-starts reg-ends scanner whole-string-output)
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
                    (length-change))
                (offset-loop
                  (current-replacement replacement-list)
                  ((setf length-change (round length-difference replacements)
                         length-difference (- length-difference length-change)
                         replacements (- replacements 1)))
                  ((destructuring-bind (start end) current-replacement
                     (round (+ start end) 2)))
                  ((output-offset) (max 0 (+ output-offset length-change)))))))
          (multiple-value-bind (hyphenated-string hyphenation-list) (hyphenate-string whole-string-output)
            (setf whole-string-output hyphenated-string)
            (offset-loop
              (current-hyphenation hyphenation-list)
              ()
              (current-hyphenation)
              ((output-offset) (1+ output-offset))))))
      (let ((current-offset 0))
        (plump:traverse
          root
          (lambda (node)
            (typecase node
              (plump:text-node
                (let ((next-offset (if offset-list (+ current-offset (first offset-list)) nil)))
                  (setf (plump:text node) (subseq whole-string-output current-offset next-offset)
                        current-offset next-offset
                        offset-list (cdr offset-list))))))))))
  root)

(define-lmdb-memoized clean-html (:sources ("src/clean-html.lisp" "src/links.lisp" "text-clean-regexps.js" "html-clean-regexps.js")) (in-html &key with-toc post-id)
  (labels ((tag-is (node &rest args)
		   (declare (type plump:node node)
			    (dynamic-extent args))
		   (let ((tag (plump:tag-name node)))
		     (some (lambda (x) (string= tag x))
			   args))) 
	   (only-child-is (node &rest args)
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
                   (plump:insert-before node e))))
	   (class-is-not (node &rest args)
			 (declare (type plump:node node)
				  (dynamic-extent args))
			 (or
			   (typep node 'plump:root)
			   (and (not (intersection (split-sequence #\Space (or (plump:attribute node "class") "")) args :test #'string=))
				(or (null (plump:parent node)) (apply #'class-is-not (cons (plump:parent node) args))))))
	   (text-node-is-not (node &rest args)
			     (declare (type plump:node node) 
				      (dynamic-extent args)) 
			     (or
			       (typep (plump:parent node) 'plump:root)
			       (every (lambda (x) (string/= (plump:tag-name (plump:parent node)) x)) args)))
	   (text-class-is-not (node &rest args)
			      (declare (type plump:node node)
				       (dynamic-extent args))
			      (apply #'class-is-not (cons (plump:parent node) args)))
	   (string-is-whitespace (string)
				 (every (lambda (c) (cl-unicode:has-binary-property c "White_Space")) string))
           (first-non-whitespace-child (node)
             (loop for e across (plump:children node)
                   when (or (typep e 'plump:element) (not (string-is-whitespace (plump:text e)))) return e))
	   (scan-for-urls (text-node)
			  (declare (type plump:text-node text-node)) 
			  (let ((text (plump:text text-node)))
			    (multiple-value-bind (url-start url-end) (ppcre:scan "(https?://[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+|[-a-zA-Z0-9.]+\\.(com|edu|gov|mil|net|org|biz|info|name|museum|us|ca|uk))(\\:[0-9]+){0,1}(/[-a-zA-Z0-9.,;:?'\\\\+&%$#=~_/]*)?" text)
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
                                        (plump:attribute new-a "href") (or (convert-any-link url :if-error :direct-link) url))
				  (plump:make-text-node new-a (clean-text url-raw))
				  (when new-text
				    (scan-for-urls new-text)
				    (setf (plump:text new-text) (clean-text (plump:text new-text))))
				  (loop for item across other-children
					do (plump:append-child (plump:parent text-node) item)))))))
	   (contents-to-html (contents min-header-level)
			     (declare (type cons contents)) 
			     (format nil "<div class=\"contents\"><div class=\"contents-head\">Contents</div><ul class=\"contents-list\">~{~A~}</ul></div>"
				     (map 'list (lambda (x) (destructuring-bind (elem-level text id) x
							      (format nil "<li class=\"toc-item-~A\"><a href=\"#~A\">~A</a></li>"
								      (- elem-level (- min-header-level 1)) id text)))
					  contents)))
	   (style-hash-to-html (style-hash)
			       (declare (type hash-table style-hash))
			       (let ((style-list (alexandria:hash-table-keys style-hash)))
				 (if style-list
				   (format nil "<style>~{~A~}</style>" style-list)
				   ""))))
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
	      (style-hash (make-hash-table :test 'equal)))
          (let ((wayward-li-container nil))
            (plump:traverse root (lambda (node)
                                   (cond
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
                                        (plump:append-child wayward-li-container node)))))
                            :test #'plump:element-p))
          (loop while (and (= 1 (length (plump:children root))) (typep (plump:first-child root) 'plump:element) (tag-is (plump:first-child root) "div"))
                do (setf (plump:children root) (plump:children (plump:first-child root)))) 
          (plump:traverse root (lambda (node)
                                 (typecase node
                                   (plump:text-node 
                                     (when (text-node-is-not node "a" "style" "pre")
                                       (scan-for-urls node))
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
                                                          (when (or aggressive-deformat (search "font-family" style) (search "font-style: inherit" style)
                                                                    (search "MsoNormal" (plump:attribute node "class")))
                                                            (setf aggressive-deformat t) 
                                                            (plump:remove-attribute node "style"))) 
                                     (when (and aggressive-deformat (tag-is node "div"))
                                       (setf (plump:tag-name node) "p"))
                                     (when (search "mjx-math" (plump:attribute node "class"))
                                       (loop for current = node then (plump:parent current)
                                             for parent = (plump:parent current)
                                             when (plump:root-p parent)
                                               do (progn (add-class current "mathjax-inline-container")
                                                         (return))
                                             when (loop for s across (plump:family current)
                                                        unless (or (eq s current)
                                                                   (and (plump:text-node-p s) (string-is-whitespace (plump:text s))))
                                                          return t)
                                               do (progn (add-class current "mathjax-inline-container")
                                                         (return))
                                             when (tag-is parent "p" "blockquote" "div")
                                               do (progn (add-class parent "mathjax-block-container")
                                                         (return))))
                                     (cond
                                       ((tag-is node "a")
                                        (let ((href (plump:attribute node "href")))
                                          (when href
                                            (let ((new-link (or (convert-any-link href :if-error :direct-link) href)))
                                              (when new-link
                                                (setf (plump:attribute node "href") new-link)))))
                                        (when (only-child-is node "u")
                                          (setf (plump:children node) (plump:children (plump:first-child node)))))
                                       ((tag-is node "img")
                                        (when
                                          (every (lambda (a) (if-let (attr (plump:attribute node a)) (ignore-errors (<= (parse-integer attr) 1)))) (list "width" "height"))
                                          (plump:remove-child node))
                                        (when
                                          (string= "/" (plump:attribute node "src") :end2 1)
                                          (setf (plump:attribute node "src") (concatenate 'string "https://www.lesswrong.com" (plump:attribute node "src")))))
                                       ((tag-is node "p" "blockquote" "div")
                                        (if (string-is-whitespace (plump:text node))
                                            (if (plump:get-elements-by-tag-name node "img")
                                                (add-class node "imgonly")
                                                (plump:remove-child node)))
                                        (when (tag-is node "blockquote")
                                          (unless (every (lambda (n) (and (plump:element-p n) (tag-is n "p"))) (plump:children node))
                                            (let ((new-p (plump:make-element node "p")))
                                              (plump:remove-child new-p)
                                              (setf (plump:children new-p) (plump:clone-children node t new-p)
                                                    (plump:children node) (plump:make-child-array))
                                              (plump:append-child node new-p)))
                                          (loop while (if-let (next-node (plump:next-sibling node))
                                                        (when (and (plump:element-p next-node) (tag-is next-node "blockquote"))
                                                          (plump:remove-child next-node)
                                                          (setf (plump:tag-name next-node) "p")
                                                          (plump:append-child node next-node))))))
                                       ((tag-is node "u")
                                        (when (only-child-is node "a")
                                          (plump:replace-child node (plump:first-child node)))) 
                                       ((tag-is node "li")
                                        (when (let ((c (plump:first-child node))) (and c (if (plump:text-node-p c) (not (string-is-whitespace (plump:text c))) (not (tag-is c "p" "ul" "ol")))))
                                          (let ((p (plump:make-element node "p")))
                                            (plump:remove-child p)
                                            (setf (plump:children p) (plump:clone-children node t p)
                                                  (plump:children node) (plump:make-child-array))
                                            (plump:append-child node p))))
                                       ((ppcre:scan "^h[1-6]$" (plump:tag-name node))
                                        (cond
                                          ((string-is-whitespace (plump:text node))
                                           (plump:remove-child node))
                                          ((plump:get-elements-by-tag-name node "p")
                                           (loop for c across (reverse (plump:children node))
                                                 do (plump:insert-after node c))
                                           (plump:remove-child node))
                                          (with-toc
                                            (incf section-count) 
                                            (unless (plump:attribute node "id") (setf (plump:attribute node "id") (format nil "section-~A" section-count))) 
                                            (let ((header-level (parse-integer (subseq (plump:tag-name node) 1))))
                                              (setf min-header-level (min min-header-level header-level)) 
                                              (push (list header-level
                                                          (with-output-to-string (stream)
                                                            (plump:traverse node (lambda (n) (typecase n (plump:text-node (when (text-node-is-not n "style" "script") (write-string (plump:text n) stream)))))))
                                                          (plump:attribute node "id"))
                                                    contents)))))
                                       ((tag-is node "style")
                                        (let ((text (plump:text node)))
                                          (when (search ".mjx-math" text)
                                            (setf (gethash text style-hash) t)))
                                        (plump:remove-child node))
                                       ((tag-is node "script")
                                        (plump:remove-child node)))))))
          (loop for node across (plump:children root)
                do (clean-dom-text node))
          (concatenate 'string (if (>= section-count 3) (contents-to-html (nreverse contents) min-header-level) "") 
                       (style-hash-to-html style-hash) 
                       (plump:serialize root nil)))))))
