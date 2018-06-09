# PROTEST/FTYPE

## Summary

The package `PROTEST/FTYPE` provides a single helper function,
`FUNCTION-FTYPE-DECLARATION-FORM`, which takes a single function lambda list
annotated with types, a function result type and a list of keyword types, and
produces a form suitable for usage in `FTYPE` declarations.

## Internal dependencies

None.

## Exports

  * **Function `FUNCTION-FTYPE-DECLARATION-FORM`**

    Given a lambda list annotated with types and a function result type,
    produces a form suitable for usage inside FTYPE declarations.

    This function is used by `PROTEST/PROTOCOL` for producing FTYPE declarations.
