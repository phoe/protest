# PROTEST/TEST-CASE

## Summary

The package `PROTEST/TEST-CASE` implements the concept of a test case as an
object describing the flow of a test.

A test case in PROTEST/TEST-CASE consists of three elements: metadata about a
given test phase, such as description and tags, test phases (denoted by
keywords), and test steps (denoted by numbers and strings). PROTEST/TEST-CASE
also implements the concept of a test that is an instance of a particular test
case.

Each test case in PROTEST/PROTOCOL has a name, which may be a string or a
symbol. All names are internally coerced to strings.

Each test phase is required to be a keyword. These are meant to describe the
group of test steps that come afterwards it.

Each test step is required to consist of a positive integer and a string
description of the step. The steps are required to be unsigned-bytes in
increasing order.

Inside a test body, the reader macro `#N?` with a numerical argument may be used
to denote a form belonging to a given test step. That information is then used
inside the test body to inform the user of which exactly test step has failed.
The implementation of that reader macro is delegated to modules integrating with
testing libraries:

  * [`PROTEST/PARACHUTE`](parachute.md)
  * [`PROTEST/5AM`](5am.md)
  * [`PROTEST/PROVE`](prove.md)
  * [`PROTEST/1AM`](1am.md)

## Internal Dependencies

  * [`PROTEST/BASE`](base.md)

## Exports

### Classes

  * **Class `TEST-CASE`**

    Describes a test case understood as a metaobject describing the metadata,
    steps and phases of a particular test.

    Each test case is uniquely identified by

    Accessors:
    * **Reader `NAME`** - returns the string that names the test case.
    * **Reader `PACKAGE-OF`** - returns the package the test case belongs to.
    * **Accessor `WHOLE`** - accesses the `DEFINE-TEST-CASE` form that the test
      case was defined with.
    * **Accessor `TAGS`** - accesses the list of tags of the test case.
    * **Accessor `ATTACHMENTS`** - accesses the list of attachments of the test
      case.
    * **Accessor `STEPS`** - accesses the hash-table containing the steps of the
      test case.
    * **Reader `STEPS-LIST`** - returns a fresh list containing the steps of the
      test case. The list is not ordered.

  * **Class `TEST-STEP`**

    Describes a test step understood as a single action performed within a test,
    identified by its number, having a string description, and optionally
    belonging to a particular test phase.

    Accessors:
    * **Reader `ID`** - returns the numeric identifier of the test step.
    * **Reader `DESCRIPTION`** - returns the textual description of the test
      step.
    * **Reader `TEST-PHASE`** - returns the symbol denoting the test phase of
      the test step.

### Documentation Types

  * **Documentation Type `TEST-CASE`**

### Functions

  * **Function `FIND-TEST-CASE`**

    Syntax: `(find-test-case NAME &optional (PACKAGE *PACKAGE*))`

    Returns a test case object matching the provided NAME and PACKAGE. If no
    such test case was found, returns NIL.

  * **Function `(SETF FIND-TEST-CASE)`**

    Syntax: `(setf (find-test-case NAME &optional (PACKAGE *PACKAGE*))
                   new-value)`

    Sets the test case object matching the provided NAME and PACKAGE.

### Macros

  * **Macro `DEFINE-TEST-CASE`**

    Syntax: `(define-test-case NAME (&rest OPTIONS) &body STEPS)`

    Defines the test case named `NAME` with the provided `OPTIONS`, containing
    the provided `STEPS`.

    `OPTIONS` is a plist containing the options selected for the test case. The
    following options are defined:

    * **`:DOCUMENTATION`**

      Expects a string that will be attached to the test case's name as a
      documentation string of documentation type `TEST-CASE`.

    * **`:TAGS`**

      Expects a list of keywords which name tags attached to this test case.

      This information is currently not processed, but will be utilized in the
      future when this library supports HTML generation.

    * **`:ATTACHMENTS`**

      Expects a list of strings naming files attached to this test case.

      This information is currently not processed, but will be utilized in the
      future when this library supports HTML generation.

## Example

An example test case and a test written in
[`PROTEST/PARACHUTE`](parachute.md):

```common-lisp
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
```

Executing test `SELF-TEST` will produce the following output:

```
             ？ CL-USER::SELF-TEST
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
       0.085 ✘ CL-USER::SELF-TEST

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
