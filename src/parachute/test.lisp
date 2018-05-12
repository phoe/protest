;;;; src/parachute/test.lisp

(in-package #:protest/for-parachute)

(in-readtable protest/parachute)

(define-test-case self-test ()
  :first-three
  1 "This test case tests TRUE."
  2 "This test case tests FALSE."
  3 "This test case tests IS EQ."
  :middle-three
  4 "This test case tests ISNT EQ."
  5 "This test case tests IS-VALUES =."
  6 "This test case tests ISNT-VALUES =."
  :last-three
  7 "This test case is expected to FAIL."
  8 "This test case tests OF-TYPE."
  9 "This test case tests FINISH.")

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

#|
？ PROTEST/FOR-PARACHUTE::SELF-TEST
#   Phase :FIRST-THREE
1   0.000 ✔   (true (numberp 2/3))
2   0.000 ✔   (false (numberp :keyword))
3   0.000 ✔   (is eq (not ()) t)
#   Phase :MIDDLE-THREE
4   0.000 ✔   (is eq (not ()) ())
5   0.000 ✔   (is-values (values 0 "1") (= 0) (equal "1"))
6   0.000 ✔   (isnt-values (values 1 :zero) (= 0) (equal "1"))
#   Phase :LAST-THREE
7   0.000 ✘   (fail :failure error)
8   0.000 ✔   (of-type string "a")
9   0.000 ✔   (finish :finish)
0.085 ✘ PROTEST/FOR-PARACHUTE::SELF-TEST

;; Summary:
Passed:     8
Failed:     1
Skipped:    0

;; Failures:
1/   9 tests failed in PROTEST/FOR-PARACHUTE::SELF-TEST
In test case SELF-TEST, phase LAST-THREE, step 7:
The test form   (capture-error :failure)
evaluated to    ()
when            error
was expected to be equal under TYPEP.
This test case is expected to FAIL.
|#
