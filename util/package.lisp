;;;; package.lisp

(defpackage #:cl-protest
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
           #:define-test
           #:*protocols*
           #:*test-cases*))

(defpackage #:cl-protest-web
  (:use #:cl
        #:alexandria
        #:cl-who
        #:ningle
        #:cl-protest))

(uiop:define-package #:cl-protest-tests
    (:use))
