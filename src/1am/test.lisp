;;;; src/1am/test.lisp

;;; TODO move into PROTEST/TEST/1AM

(in-package #:protest/1am)

(in-readtable protest/1am)

(define-test-case self-test ()
  :foo
  1 "This test case tests IS."
  :bar
  2 "This test case tests SIGNALS.")

(define-test self-test
  #1?(is (= 4 (+ 2 2)))
  #2?(signals error (error "error")))
