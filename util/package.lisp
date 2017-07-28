;;;; package.lisp

(defpackage #:protest
  (:use #:cl
        #:alexandria
        #:named-readtables
        #:1am
        #:closer-mop)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:export #:define-protocol
           #:define-test-case
           #:define-test-package
           #:define-test
           #:*protocols*
           #:*test-cases*))

(defpackage #:protest-web
  (:use #:cl
        #:alexandria
        #:cl-who
        #:ningle
        #:protest))

(uiop:define-package #:protest-tests
    (:use))
