# PROTEST/BASE

## Summary

The package `PROTEST/BASE` exports the base functionality useful
for manual definition of protcols.

The macros `DEFINE-PROTOCOL-CLASS` and `DEFINE-PROTOCOL-CONDITION-TYPE` can be
used for defining protocol classes and protocol condition types, while condition
type `PROTOCOL-OBJECT-INSTANTIATION` can be handled specifically to handle
errors that arise when users attempt to instantiate these protocol objects.

Additionally, the condition type `PROTOCOL-ERROR` can be handled to handle all
errors that arise regarding protocol and test-case definition. This condition
type is later reused in other PROTEST packages.

## Internal dependencies

None. This package can be used standalone.

## Exported symbols:

  * **Macro `DEFINE-PROTOCOL-CLASS`**

    Like `DEFCLASS`, but instances of the defined class will not be directly
    instantiable.

  * **Macro `DEFINE-PROTOCOL-CONDITION-TYPE`**

    Like `DEFINE-CONDITION`, but instances of the defined condition type will
    not be directly instantiable.

    *SBCL Note: This constraint is not enforced on SBCL. See
    [this](https://bugs.launchpad.net/sbcl/+bug/1761735) for details.

  * **Condition Type `PROTOCOL-ERROR`**

    Parent condition type of all errors related to protocols.

  * **Condition Type `SIMPLE-PROTOCOL-ERROR`**

    Condition type of supertypes `PROTOCOL-ERROR` and `SIMPLE-CONDITION`.

  * **Condition Type `PROTOCOL-OBJECT-INSTANTIATION`**

    Error type signaled whenever protocol objects (such as protocol classes and
    protocol condition types) are attempted to be instantiated.

  * **Reader `PROTOCOL-OBJECT-INSTANTIATION-SYMBOL`**

    Returns the name of the object that was attempted to be instantiated when
    `PROTOCOL-OBJECT-INSTANTIATION` was signaled.

  * **Reader `PROTOCOL-OBJECT-INSTANTIATION-TYPE`**

    Returns the human-readable representation of type of the object that was
    attempted to be instantiated when `PROTOCOL-OBJECT-INSTANTIATION` was signaled.
