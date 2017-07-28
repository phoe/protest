(define-test-case test-frob ()
  :arrange
  1 "Make a fresh BAR."
  2 "Make a fresh BAZ."
  :act
  3 "Frob BAR and BAZ."
  :assert
  4 "Assert BAZ is frobbed."
  5 "Assert BAR is frobbed.")

(define-test test-frob ()
  (let ((bar #1?(make-bar))
        (baz #2?(make-baz)))
    #3?(frob bar baz)
    #4?(assert (frob-p bar))
    #5?(assert (frob-p baz))))

#|
Test failure in test TEST-FROB, phase ASSERT, step 4: Assert BAR is frobbed."
Error: Assertion (FROB-P BAR) failed.
|#
