;;;; t/1am.lisp

(defpackage #:protest/test/1am
  (:use #:cl
        #:named-readtables
        #:protest
        #:protest/1am)
  (:export #:self-test))

(in-package #:protest/test/1am)

(in-readtable protest/1am)

(define-test-case self-test ()
  :foo
  1 "This test case tests IS."
  :bar
  2 "This test case tests SIGNALS.")

(define-test self-test
  #1?(is (= 4 (+ 2 2)))
  #2?(signals error (error "error")))
