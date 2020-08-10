(uiop:define-package #:lw2.data-viewers.tag
    (:use #:cl #:lw2.schema-type #:lw2.backend-modules))

(in-package #:lw2.data-viewers.tag)

(define-schema-type :tag ()
  ((tag-id string :alias :--id)
   (name string)
   (slug string)
   (post-count (or null fixnum))
   (description list
		:context :body
		:subfields (:html))))
