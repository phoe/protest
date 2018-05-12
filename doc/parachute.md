# PROTEST/PARACHUTE

## Summary

The package `PROTEST/PARACHUTE` provides the functionality of the
[Parachute](https://github.com/Shinmera/parachute/) testing framework wrapped by
the PROTEST/TEST-CASE test case definition facility. Its main function is
creating a thin layer around test assertions that, in case of failure, informs
the user of the details of the failed test step.

PROTEST/PARACHUTE is designed as a drop-in replacement for the original
Parachute. It should be enough to replace `:PARACHUTE` with
`:PROTEST/PARACHUTE` in your package use lists and write the test cases for all
implemented tests. (TODO rethink this, most likely we want tests without test
cases)

## Internal Dependencies

  * [`PROTEST/BASE`](base.md)
  * [`PROTEST/TEST-CASE`](test-case.md)

## Exports

Same as Parachute, except:

  * **Readtable `PROTEST/PARACHUTE`**

  This readtable implements the `#?` reader macro. It is required to be in this
  readtable to use this macro to define test step implementations.

## Example

To run the example test, load the PROTEST/PARACHUTE system and evaluate
`(protest/for-parachute::invoke-test)`. The results should look
like:

```common-lisp
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
       0.081 ✘ PROTEST/FOR-PARACHUTE::SELF-TEST

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
```
