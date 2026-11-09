(uiop:define-package #:lw2.thread-utils
  (:use #:cl)
  (:import-from #:sb-thread
		#:with-mutex)
  (:import-from #:sb-sys
		#:with-deadline)
  (:export #:with-mutex-no-deadline))

(in-package #:lw2.thread-utils)

(defmacro with-mutex-no-deadline ((&rest args) &body body)
  `(with-deadline (:override t)
     (with-mutex ,args ,@body)))
