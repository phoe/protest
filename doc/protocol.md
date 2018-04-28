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

  * **Option `:DECLAIM-TYPES-P`**

    States if the types and ftypes of the elements should be declaimed when the
    protocol is executed. If not supplied, it defaults to `T`.

## Protocol elements

TODO accessors for each protocol element class

Syntax summary of all configuration elements:

```
(:class NAME SUPERCLASSES SLOTS . OPTIONS)
(:condition-type NAME SUPERTYPES SLOTS . OPTIONS)
(:function NAME LAMBDA-LIST &optional RETURN-TYPE KEYWORD-TYPES)
(:macro NAME LAMBDA-LIST)
(:variable NAME &optional VALUE-TYPE INITIAL-VALUE)
(:category NAME)
(:config NAME &optional VALUE-TYPE MANDATORYP INITIAL-VALUE)
```

### :CLASS

Describes a protocol class that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CLASS`.

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

These elements are represented by instances of class `PROTOCOL-CONDITION-TYPE`.

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

These elements are represented by instances of class `PROTOCOL-FUNCTION`.

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
  * **`KEYWORD-TYPES`** - must be a valid plist containing some or all of the
  `&KEY` arguments used in `LAMBDA-LIST` along with their respective types.

This element expands into `DEFGENERIC` with the exception that already existing
generic functions are not redefined. (TODO reconsider this) The type information
from `LAMBDA-LIST`, `RETURN-TYPE` and `KEYWORD-TYPES` will be passed to a
`DECLAIM FTYPE` call if the protocol is defined with `:DECLAIM-TYPES-P` being
true.

### :MACRO

Describes a macro that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-MACRO`.

The form obeys the following grammar:

```
(:macro NAME LAMBDA-LIST)
```

  * **`NAME`** - must be a symbol. Denotes the name of the macro.
  * **`LAMBDA-LIST`** - must be a valid macro lambda list.

This element does not expand into anything.

### :VARIABLE

Describes a variable that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-VARIABLE`.

The form obeys the following grammar:

```
(:variable NAME &optional VALUE-TYPE INITIAL-VALUE)
```

  * **`NAME`** - must be a symbol. Denotes the name of the variable.
  * **`VALUE-TYPE`** - must be a valid type specifier. Denotes the type that the
    variable is allowed to take.
  * **`INITIAL-VALUE`** - denotes the default value that the variable will have
    at the moment of executing the protocol. If not passed, the variable will be
    unbound.

This element expands into `DEFVAR`. The type information from `VALUE-TYPE` will
be passed to a `DECLAIM TYPE` call if the protocol is defined with
`:DECLAIM-TYPES-P` being true.

### :CATEGORY

Describes a configuration category that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CATEGORY`.

The form obeys the following grammar:

```
(:category NAME)
```

  * **`NAME`** - must be a non-empty list of keywords. Denotes the name of the
    configuration category. The names of configuration categories and
    configuration entries must not collide with each other.

This element expands into a call to the value of `*CATEGORY-CALLBACK*` with the
category's name being the argument to that function.

### :CONFIG

Describes a configuration entry that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CONFIG`.

The form obeys the following grammar:

```
(:config NAME &optional VALUE-TYPE MANDATORYP INITIAL-VALUE)
```

  * **`NAME`** - must be a non-empty list of keywords. Denotes the name of the
    configuration entry. The names of configuration categories and configuration
    categories must not collide with each other.
  * **`VALUE-TYPE`** - must be a proper type designator. Denotes the type that
    the configuration entry is allowed to take. If not specified, it will
    default to `T`.
  * **`MANDATORYP`** - must be either `:MANDATORY` or `:OPTIONAL`. States if the
    configuration entry is mandatory to be set before any client code is allowed
    to be executed. If not specified, it will default to `:OPTIONAL`.
  * **`INITIAL-VALUE`** - denotes the default value that the configuration entry
    should have at the moment of executing the protocol. If not passed, the
    value will not be set.

This element expands into a call to the value of `*CONFIG-CALLBACK*` with the
configuration entry's `NAME`, `VALUE-TYPE`, `MANDATORYP` as required parameters
and `INITIAL-VALUE` as an optional parameter.

## Example

```common-lisp
(define-protocol fuelable
    (:documentation "Defines objects which have a fuel tank and must be refueled
to function."
     :tags (:industrial :electricity :coal :oil)
     :export t)
  (:function fuel ((object fuelable)) real)
  "Returns the current amount of fuel in the fuelable."
  (:function (setf fuel) ((new-value real) (object-fuelable)) real)
  "Sets the current amount of fuel in the fuelable.")

(define-protocol automobile
    (:documentation "Defines objects which are able to move forward by means of
an engine and can hold people and luggage."
     :tags (:industrial :machine)
     :dependencies (fuelable)
     :export t)
  (:class automobile () ())
  "An automobile object. Objects participaring in this protocol must subclass
this protocol class."
  (:function brand ((object automobile)) keyword)
  "Returns the brand of a automobile."
  (:function (setf brand) ((new-value keyword) (object automobile)) keyword)
  "Sets the brand of an automobile."
  (:function drive ((object automobile) (distance real) &optional env) (values))
  "Drives a given distance with the given automobile. If ENV is supplied, the
drive occurs in that environment."
  (:macro with-snow-tires ((tire-brand) &body body))
  "Executes the body with snow tires on all automobiles, therefore diminishing
chances of an accident in snow environment."
  (:variable *accident-chance* (float 0.0 1.0) 0.005)
  "The chance of an accident per 100 kilometers. This variable is expected to
be rebound when the environment changes."
  (:category :automobile)
  "Describes configuration entries related to all automobiles in general."
  (:config (:automobile :maximum-lead-per-100-kilometers)
           (float 0.0) :optional 0.0005)
  "Describes how many grams of lead an engine is allowed to output after driving
for 100 kilometers."
  (:category (:automobile :steering))
  "Describes configuration entries related to the automobiles' steering."
  (:config (:automobile :steering :wheel-side) (member :left :right :any)
           :mandatory)
  "Describes if the automobiles must have steering wheels on left or right
side.")
```

The code above defines two protocols, `FUELABLE` and `AUTOMOBILE`. The protocol
objects are now available in the `*PROTOCOLS*` hash-table for introspection.

Executing these two protocols produces the following side effects:

```common-lisp
;;;; Protocol FUELABLE
(setf (documentation 'fuelable 'protocol)
      "Defines objects which have a fuel tank and must be refueled
to function.")

(defgeneric fuel (object)
  (:documentation "Returns the current amount of fuel in the fuelable."))
(declaim (ftype (function (fuelable) real) fuel))

(defgeneric (setf fuel) (new-value object)
  (:documentation "Sets the current amount of fuel in the fuelable."))
(declaim (ftype (function (real fuelable) real) (setf fuel)))

(export '(fuel))

;;;; Protocol AUTOMOBILE
(setf (documentation automobile 'protocol)
      "Defines objects which are able to move forward by means of an
engine and can hold people and luggage.")

(define-protocol-class automobile () nil
  (:documentation "An automobile object. Objects participaring in this protocol
must subclass this protocol class."))

(defgeneric brand (object)
  (:documentation "Returns the brand of a automobile."))
(declaim (ftype (function (automobile) keyword) brand))

(defgeneric (setf brand) (new-value object)
  (:documentation "Sets the brand of a automobile."))
(declaim (ftype (function (keyword automobile) keyword) (setf brand)))

(defgeneric drive (object distance &optional env)
  (:documentation "Drives a given distance with the given automobile. If ENV
is supplied, the drive occurs in that environment."))
(declaim (ftype (function (automobile real t) (values)) drive))

(setf (documentation 'with-snow-tires 'function)
      "Executes the body with snow tires on all automobiles, therefore
diminishing chances of an accident in snow environment.")

(defvar *accident-chance* 0.005)
(declaim (type (float 0.0 1.0) *accident-chance*))
(setf (documentation '*accident-chance* 'variable)
      "The chance of an accident per 100 kilometers. This variable is expected
to be rebound when the environment changes.")

(setf (documentation '(:automobile) 'category)
      "Describes configuration entries related to all automobiles in general.")

(funcall *configuration-setter*
         '(:automobile :maximum-lead-per-100-kilometers)
         '(float 0.0) nil 5.0e-4)
(setf (documentation '(:automobile :maximum-lead-per-100-kilometers) 'config)
      "Describes how many grams of lead an engine is allowed to output after
driving for 100 kilometers.")

(setf (documentation '(:automobile :steering) 'category)
      "Describes configuration entries related to the automobiles' steering.")

(setf (documentation '(:automobile :steering :wheel-side) 'config)
      "Describes if the automobiles must have steering wheels on left or right
side.")

(export '(automobile brand drive with-snow-tires *accident-chance*))
```


## Extending PROTEST/PROTOCOL

In order to introduce a new protocol element into PROTEST/PROTOCOL, define a new
class subtyping one of `PROTOCOL-ELEMENT`'s protocol subclasses - either
`PROTOCOL-OPERATION` for operations or `PROTOCOL-DATA-TYPE` for data types.

You will need to define methods on three generic functions:

  * `GENERATE-ELEMENT`, which generates the element based on its list
    representation,
  * `GENERATE-FORMS`, which accepts a protocol element and returns its list
    representation,
  * `GENERATE-CODE`, which accepts a protocol element and generates code to be
    executed whenever the protocol is executed.

For `GENERATE-ELEMENT`, pick a keyword that will represent your new element and
use it in an  EQL specializer when defining a method on `GENERATE-ELEMENT.`

You may also want to define methods on `DOCUMENTATION` and `SETF DOCUMENTATION`
for your protocol element.

An example PROTEST/PROTOCOL extension with the protocol element `:FOO` of class
`PROTOCOL-FOO` may look like this:

```common-lisp
(defclass protocol-foo (protocol-data-type)
  ...)
(defmethod generate-element ((type (eql :foo)) form &optional declaim-type-p)
  ...)
(defmethod generate-forms ((element protocol-foo))
  ...)
(defmethod generate-code ((element protocol-foo))
  ...)
(defmethod documentation (slotd (doc-type (eql 'foo)))
  ...)
(defmethod (setf documentation) (new-value slotd (doc-type (eql 'foo)))
  ...)
```
