;;;; package.lisp

(defpackage #:cltms
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:use #:cl
        #:alexandria
        #:closer-mop)
  (:export #:define-protocol
           #:define-test-case
           #:*protocols*
           #:*test-cases*))
