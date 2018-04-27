# PROTEST/PROTOCOL

## Summary

The package `PROTEST/PROTOCOL` provides the functionality for defining protocol
objects and executing them for side effects. The protocol objects may have
declarations and documentation for protocol classes, generic functions,
variables, etc., and executing a protocol may mean executing, among others,
`DEFCLASS`, `DEFGENERIC`, `DEFVAR`, `SETF DOCUMENTATION`, `DECLAIM TYPE`,
`DECLAIM FTYPE`, etc..

(For a formal definition of a protocol, see the related work by
[Robert Strandh](http://metamodular.com/protocol.pdf).)

PROTEST implements the concept of a protocol with operations being defined as
generic functions and macros, and data types being, among others, protocol
classes, protocol condition types, and special variables.

For utility, PROTEST also describes configuration categories and entries,
defined as list of keywords and non-keyword symbols, along with the type that
the value of each configuration entry is allowed to take.

A protocol in PROTEST consists of metadata about a given protocol, such as
description and tags, and a series of protocol element definitions.

Each element of the protocol is described by a list whose first element is the
keyword denoting the type of a given element, and the rest contain the arguments
that will be passed to that element's constructor.

## Internal dependencies

  * [`PROTEST-BASE`](base.md)

## Exports

  * **Class `PROTOCOL`**



  * **Macro `DEFINE-PROTOCOL`**

    ```
    (define-protocol NAME (&rest OPTIONS) . ELEMENTS-AND-DOCSTRINGS)
    ```

## Syntax

TODO documentation strings

## Options

  * **Option `:DOCUMENTATION`**

    Expects a string that will be attached to the protocol's name as a
    documentation string of documentation type `PROTOCOL`.

  * **Option `:TAGS`**

    Expects a list of keywords which name tags attached to this protocol.

    This information is currently not processed, but will be utilized in the
    future when this library supports HTML generation.

  * **Option `:ATTACHMENTS`**

    Expects a list of strings naming files attached to this protocol.

    This information is currently not processed, but will be utilized in the
    future when this library supports HTML generation.

  * **Option `:DEPENDENCIES`**

    Expects a list of symbols that name other protocols. These protocols are
    declared dependencies of the current protocol. Defining a protocol whose
    dependencies are not defined is an error.

    An error is signaled if the protocols have colliding elements, such as a
    pair of generic functions with the same name. Errors are also signaled in
    case of erroneous dependency definition, such as circular dependencies.

  * **Option `:EXPORT`**

    Expects a list of all symbols that are meant to be exported when the
    protocol is executed, or the symbol `T`, meaning that the names of all
    protocol elements are going to be exported. This includes names of `SETF`
    functions but does not include names which are lists of keywords, such as
    the names for configuration entries and categories.

## Protocol elements

TODO accessors for each protocol element class

### :CLASS

Describes a protocol class that is a part of a protocol.

Elements of this type are of class `PROTOCOL-CLASS`.

The form for obeys the following grammar:

```
(:class NAME SUPERCLASSES SLOTS . OPTIONS)
```

  * **`NAME`** - must be a symbol. Denotes the name of the class.
  * **`SUPERCLASSES`** - must be a list of symbols. Denotes the superclasses of
    the class.
  * **`SLOTS`** - must be a list of slot definitions. Denotes the slots of the
    class. It is discouraged to create slots in protocol classes; client code
    should instead create slots in concrete classes which subclass the protocol
    classes.
  * **`OPTIONS`** - denotes the options that will be passed to
    `DEFINE-PROTOCOL-CLASS`.

This element expands into `DEFINE-PROTOCOL-CLASS`.

### :CONDITION-TYPE

Describes a protocol condition type that is a part of a protocol.

Elements of this type are of class `PROTOCOL-CONDITION-TYPE`.

The form for obeys the following grammar:

```
(:condition-type NAME SUPERTYPES SLOTS . OPTIONS)
```

  * **`NAME`** - must be a symbol. Denotes the name of the condition type.
  * **`SUPERTYPES`** - must be a list of symbols. Denotes the supertypes of the
    condition type.
  * **`SLOTS`** - must be a list of slot definitions. Denotes the slots of the
    condition type. It is discouraged to create slots in protocol condition
    types; client code should instead create slots in concrete condition types
    which subtype the protocol condition types.
  * **`OPTIONS`** - denotes the options that will be passed to
    `DEFINE-PROTOCOL-CONDITION-TYPE`.

This element expands into `DEFINE-PROTOCOL-CONDITION-TYPE`.

### :FUNCTION

Describes a generic function that is a part of a protocol.

Elements of this type are of class `PROTOCOL-FUNCTION`.

The form obeys the following grammar:

```
(:function NAME LAMBDA-LIST &optional RETURN-TYPE KEYWORD-TYPES)
```

  * **`NAME`** - must be a symbol or a `SETF` form. Denotes the name of the
    function.
  * **`LAMBDA-LIST`** - must be a valid lambda list for the function.
    Additionally, this lambda list may contain type information for required and
    optional arguments in form `(ARG-NAME TYPE)`.
  * **`RETURN-TYPE`** - must be a valid return type for a function. If not
    specified, defaults to the symbol `CL:*`.
  * **`KEYWORD-TYPES`** - optional, must be a valid plist containing some or all
    of the `&KEY` arguments used in `LAMBDA-LIST` along with their respective
    types.

This element expands into `DEFGENERIC` with the exception that already existing
generic functions are not redefined. (TODO reconsider this) The type information
from `LAMBDA-LIST`, `RETURN-TYPE` and `KEYWORD-TYPES` will be passed to a
`DECLAIM FTYPE` call if the protocol is executed with `*DECLAIM-TYPES*` being
true.

### :MACRO

Describes a macro that is a part of a protocol.

Elements of this type are of class `PROTOCOL-MACRO`.

The form obeys the following grammar:

```
(:macro NAME LAMBDA-LIST)
```

  * **`NAME`** - must be a symbol. Denotes the name of the macro.
  * **`LAMBDA-LIST`** - must be a valid macro lambda list.

This element does not expand into anything.

### :VARIABLE

### :CATEGORY

### :CONFIG

## Example
