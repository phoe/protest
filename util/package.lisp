;;;; package.lisp

(defpackage #:cl-protest
  (:use #:cl
        #:alexandria
        #:named-readtables
        #:closer-mop)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:export #:define-protocol
           #:define-test-case
           #:*protocols*
           #:*test-cases*))

(defpackage #:cl-protest-web
  (:use #:cl
        #:alexandria
        #:cl-who
        #:ningle
        #:cl-protest))
