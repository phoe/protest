# PROTEST/1AM

## Summary

The package `PROTEST/1AM` provides the functionality of the
[1AM](https://github.com/lmj/1am/) testing framework wrapped by the
PROTEST/TEST-CASE test case definition facility. Its main function is creating a
thin layer around test assertions that, in case of failure, informs the user of
the details of the failed test step.

PROTEST/1AM is designed as a drop-in replacement for the original 1AM. It should
be enough to replace `:1AM` with `:PROTEST/1AM` in your package use lists and
write the test cases for all implemented tests.

## Internal Dependencies

  * [`PROTEST/BASE`](base.md)
  * [`PROTEST/TEST-CASE`](test-case.md)

## Exports

Same as 1AM, except:

  * **Macro `DEFINE-TEST`**

    Convenience macro, functionally equivalent to the macro `TEST`.

  * **Readtable `PROTEST/1AM`**

    This readtable implements the `#?` reader macro. It is required to be in this
    readtable to use this macro to define test step implementations.

## Example

To run the example test, load the `PROTEST/TEST` system and evaluate
`(PROTEST/TEST/1AM::SELF-TEST)`. The results should look like:

```common-lisp
PROTEST/TEST/1AM::SELF-TEST..
Success: 1 test, 2 checks.
```

The system is also tested as a part of unit tests inside the `PROTEST/TEST` test suite.
