;;;; t/1am.lisp

(defpackage #:protest/test/1am
  (:use #:cl
        #:named-readtables
        #:protest
        #:protest/test))

(in-package #:protest/test/1am)

(register-test-package)

(in-readtable protest/1am:protest/1am)

(protest/test-case:define-test-case self-test ()
  :foo
  1 "This test case tests IS."
  :bar
  2 "This test case tests SIGNALS.")

(protest/1am:define-test self-test
  #1?(protest/1am:is (= 4 (+ 2 2)))
  #2?(protest/1am:signals error (error "error")))

(define-protest-test test-1am-self-test
  (let ((*standard-output* (make-broadcast-stream)))
    (self-test)))
