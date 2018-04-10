;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use #:common-lisp
        #:alexandria
        #:named-readtables
        #:protest/base)
  (:export #:define-test-case #:define-test #:define-testing-library))
