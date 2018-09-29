# PROTEST/BASE

## Summary

The package `PROTEST/BASE` exports the base functionality useful
for manual definition of protcols.

Protocol classes and protocol condition types are instances of Common Lisp
classes and condition types, respectively, except they must not be instantiated
directly by client code. Users of these classes must instead create subclasses
of these protocol classes or subtypes of these protocol condition types in order
for them to participate in the protocol.

The macros `DEFINE-PROTOCOL-CLASS` and `DEFINE-PROTOCOL-CONDITION-TYPE` can be
used for defining protocol classes and protocol condition types, while condition
type `PROTOCOL-OBJECT-INSTANTIATION` can be handled specifically to handle
errors that arise when users attempt to instantiate these protocol objects.

Additionally, the condition type `PROTOCOL-ERROR` can be handled to handle all
errors that arise regarding protocol and test-case definition. This condition
type is later reused in other PROTEST packages.

## Internal dependencies

None.

## Exports

  * **Macro `DEFINE-PROTOCOL-CLASS`**

    Syntax: `(define-protocol-class name superclasses
                                    slots &rest options)`

    Like `DEFCLASS`, but the defined class will not be directly instantiable.

  * **Macro `DEFINE-PROTOCOL-CONDITION-TYPE`**

    Syntax: `(define-protocol-condition-type name supertypes
                                             slots &rest options)`

    Like `DEFINE-CONDITION`, but the defined condition type will not be directly
    instantiable.

    *SBCL Note:* [This SBCL issue](https://bugs.launchpad.net/sbcl/+bug/1761735)
    prevents this constraint from being enforceable on SBCL for all condition
    instances instantiated via `MAKE-CONDITION`. If you want to avoid this
    behavior, make sure that you instantiate your conditions via `MAKE-INSTANCE`
    instead. While not being compliant with the ANSI CL standard, this method of
    instantiating conditions works correctly at the time of writing on SBCL,
    CCL, ECL, ABCL, ACL and LispWorks.

  * **Condition Type `PROTOCOL-ERROR`**

    Parent condition type of all errors related to protocols.

  * **Condition Type `SIMPLE-PROTOCOL-ERROR`**

    Condition type of supertypes `PROTOCOL-ERROR` and `SIMPLE-CONDITION`.

  * **Condition Type `PROTOCOL-OBJECT-INSTANTIATION`**

    Error type signaled whenever protocol objects (such as protocol classes and
    protocol condition types) are attempted to be instantiated.

    Accessors:
    * **Reader `PROTOCOL-OBJECT-INSTANTIATION-SYMBOL`** - returns the name of
      the object that was attempted to be instantiated when
      `PROTOCOL-OBJECT-INSTANTIATION` was signaled.
    * **Reader `PROTOCOL-OBJECT-INSTANTIATION-TYPE`** - returns the
      human-readable representation of type of the object that was attempted to
      be instantiated when `PROTOCOL-OBJECT-INSTANTIATION` was signaled.
