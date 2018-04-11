;;;; parachute/test.lisp

(in-package #:protest/for-parachute)

(in-readtable protest/parachute)

(define-test-case self-test ()
  :first-three
  1 "true"
  2 "false"
  3 "is"
  :middle-three
  4 "isnt"
  5 "is-values"
  6 "isnt-values"
  :last-three
  7 "fail"
  8 "of-type"
  9 "finish")

(define-test self-test
  #1?(true (numberp 2/3))
  #2?(false (numberp :keyword))
  #3?(is eq (not nil) t)
  #4?(isnt eq (not nil) nil)
  #5?(is-values (values 0 "1")
       (= 0)
       (equal "1"))
  #6?(isnt-values (values 1 :zero)
       (= 0)
       (equal "1"))
  #7?(fail :failure)
  #8?(of-type string "a")
  #9?(finish :finish))

(defun invoke-test ()
  (parachute:test 'self-test))
