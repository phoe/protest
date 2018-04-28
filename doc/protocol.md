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

Syntax summary of all options and configuration elements:

```common-lisp
(define-protocol SYMBOL (:documentation STRING
                         :tags KEYWORD-LIST
                         :attachments STRING-LIST
                         :dependencies PROTOCOL-NAME-LIST
                         :export SYMBOL-LIST
                         :declaim-types-p BOOLEAN)
  (:class NAME SUPERCLASSES SLOTS . OPTIONS)
  (:condition-type NAME SUPERTYPES SLOTS . OPTIONS)
  (:function NAME LAMBDA-LIST &optional RETURN-TYPE KEYWORD-TYPES)
  (:macro NAME LAMBDA-LIST)
  (:variable NAME &optional VALUE-TYPE INITIAL-VALUE)
  (:category NAME)
  (:config NAME &optional VALUE-TYPE MANDATORYP INITIAL-VALUE))
```

## Internal dependencies

  * [`PROTEST-BASE`](base.md)

## Exports

* **Variable `*PROTOCOLS*`**

    Its value is a hash-table mapping from symbols naming the protocols to the
    protocol objects themselves.

    By default, the value of `*PROTOCOLS*` is an empty hash-table.

  * **Variable `*CONFIG-CALLBACK*`**

    Its value is a function of two arguments used as a callback for declaring
    configuration values. The first argument to that function is the
    configuration entry name, the second is the type that the value of the
    configuration entry is allowed to take, the third is a boolean stating if
    the configuration entry is mandatory to be set, and the fourth is the
    initial value that was passed in the protocol. If no optional value was
    provided, this argument is not passed.

    This variable is expected to be rebound around calls to `EXECUTE-PROTOCOL`
    in order for the modified callback to take effect.

    By default, the value of `*CONFIG-CALLBACK*` is `(CONSTANTLY NIL)`.

  * **Variable `*CATEGORY-CALLBACK*`**

    Its value is a function of one argument used as a callback for declaring
    categories. The only argument to that function is the category name.

    This variable is expected to be rebound around calls to `EXECUTE-PROTOCOL`
    in order for the modified callback to take effect.

    By default, the value of `*CATEGORY-CALLBACK*` is `(CONSTANTLY NIL)`.

  * **Class `PROTOCOL`**

    Describes a protocol understood as a relation between data types and
    operations on these types.

    Accessors:
    * **Reader `NAME`** - returns the symbol that names the protocol.
    * **Accessor `WHOLE`** - accesses the `DEFINE-PROTOCOL` form that the
      protocol was defined with.
    * **Accessor `TAGS`** - accesses the list of tags of the protocol.
    * **Accessor `ATTACHMENTS`** - accesses the list of attachments of the
      protocol.
    * **Reader `DEPENDENCIES`** - returns the list of symbols which name
      protocols that this protocol depends on.
    * **Accessor `EXPORTS`** - accesses the list of symbols exported by the
      protocol.
    * **Accessor `ELEMENTS`** - accesses the list of protocol elements of the
    protocol.

  * **Protocol Class `PROTOCOL-ELEMENT`**

    Describes an abstract protocol element.

    Each protocol element must be a subclass this class.

    Accessors:
    * **Reader `NAME`** - returns the name of the element.
    * **Accessor `DOCSTRING`** - returns the documentation string attached
      to the element during protocol definition.

  * **Protocol Class `PROTOCOL-OPERATION`**

    Subclass of `PROTOCOL-ELEMENT` that describes an operation belonging to a
    protocol.

  * **Protocol Class `PROTOCOL-DATA-TYPE`**

    Subclass of `PROTOCOL-ELEMENT` that describes a data-type belonging to a
    protocol.

  * **Class `PROTOCOL-FUNCTION`**

    Subclass of `PROTOCOL-OPERATION` that describes a generic function belonging
    to a protocol. See protocol element `:FUNCTION` below.

    Accessors:
    * **Reader `NAME`** - returns the symbol naming the function.
    * **Reader `LAMBDA-LIST`** - returns the typed lambda-list of the function.
    * **Accessor `RETURN-TYPE`** - accesses the return type of the function.
    * **Accessor `KEYWORD-TYPES`** - accesses the plist mapping from some or all
      of the keyword arguments used in `LAMBDA-LIST` to the types of their
      arguments.
    * **Accessor `DECLAIM-TYPE-P`** - accesses the boolean value stating if this
      function's ftype should be proclaimed via `DECLAIM FTYPE`.

  * **Class `PROTOCOL-MACRO`**

    Subclass of `PROTOCOL-OPERATION` that describes a macro belonging to a
    protocol. See protocol element `:MACRO` below.

    Accessors:
    * **Reader `NAME`** - returns the symbol naming the macro.
    * **Reader `LAMBDA-LIST`** - returns the lambda-list of the macro.

  * **Class `PROTOCOL-CLASS`**

    Subclass of `PROTOCOL-DATA-TYPE` that describes a class belonging to a
    protocol. See protocol element `:CLASS` below.

    Accessors:
    * **Reader `NAME`** - returns the symbol naming the class.
    * **Accessor `SUPERCLASSES`** - accesses the list of all superclasses of the
      class.
    * **Accessor `SLOTS`** - accesses the list of all slot definitions of the
      class.
    * **Accessor `OPTIONS`** - accesses the list of all options of the class.

  * **Class `PROTOCOL-CONDITION-TYPE`**

    Subclass of `PROTOCOL-DATA-TYPE` that describes a condition type belonging
    to a protocol. See protocol element `:CONDITION-TYPE` below.

    Accessors:
    * **Reader `NAME`** - returns the symbol naming the condition type.
    * **Accessor `SUPERTYPES`** - accesses the list of all supertypes of the
      condition type.
    * **Accessor `SLOTS`** - accesses the list of all slot definitions of the
      condition type.
    * **Accessor `OPTIONS`** - accesses the list of all options of the
      condition type.

  * **Class `PROTOCOL-VARIABLE`**

    Subclass of `PROTOCOL-DATA-TYPE` that describes a variable belonging to a
    protocol. See protocol element `:VARIABLE` below.

    Accessors:
    * **Reader `NAME`** - returns the symbol naming the variable.
    * **Accessor `VALUE-TYPE`** - accesses the type of the value bound to the
      variable.
    * **Accessor `INITIAL-VALUE`** - accesses the default value that the
      variable will be bound to at the moment of executing the protocol.
    * **Accessor `DECLAIM-TYPE-P`** - accesses the boolean value stating if this
      variable's type should be proclaimed via `DECLAIM TYPE`.

  * **Class `PROTOCOL-CATEGORY`**

    Subclass of `PROTOCOL-DATA-TYPE` that describes a configuration category
    belonging to a protocol. See protocol element `:CATEGORY` below.

    Accessors:
    * **Reader `NAME`** - returns the list of keywords naming the category.

  * **Class `PROTOCOL-CONFIG`**

    Subclass of `PROTOCOL-DATA-TYPE` that describes a configuration entry
    belonging to a protocol. See protocol element `:CONFIG` below.

    Accessors:
    * **Reader `NAME`** - returns the list of keywords naming the category.
    * **Accessor `VALUE-TYPE`** - accesses the type of the value bound to the
      configuration entry.
    * **Accessor `MANDATORYP`** - accesses the boolean stating whether this
      configuration entry must have a value set before any client code may be
      executed.
    * **Accessor `INITIAL-VALUE`** - accesses the default value that the
      configuration entry will be bound to at the moment of executing the
      protocol.

  * **Documentation Type `PROTOCOL`**

    Names documentation strings belonging to protocol objects.

  * **Documentation Type `CATEGORY`**

    Names documentation strings belonging to configuration categories.

  * **Documentation Type `CONFIG`**

    Names documentation strings belonging to configuration entries.

  * **Generic Function `GENERATE-ELEMENT`**

    Syntax: `(generate-element TYPE DETAILS &OPTIONAL DECLAIM-TYPE-P)`

    Generates the protocol element based on its list representation.

    This function is called by `DEFINE-PROTOCOL` for each element it encounters
    in the protocol definition form. `TYPE` is bound to the head of the form and
    `DETAILS` is bound to its tail. The parameter `DECLAIM-TYPE-P` states
    whether the element is meant to declaim any types; an element which has no
    types to declaim may ignore this argument.

    This function can be freely called by the user to manually create new
    protocol elements that can later be attached to protocols.

  * **Generic Function `GENERATE-FORMS`**

    Syntax: `(generate-forms PROTOCOL-ELEMENT)`

    Generates a fresh list of forms that is suitable to be `NCONC`ed with other
    forms to generate a protocol body.

    This function is effectively an inverse of `GENERATE-ELEMENT`.

  * **Generic Function `GENERATE-CODE`**

    Syntax: `(generate-code PROTOCOL-ELEMENT)`

    Generates a fresh list of forms that is suitable to be `NCONC`ed with other
    forms to generate the Lisp code that is meant to come into effect when the
    protocol is defined.

    This function is called by `EXECUTE-PROTOCOL` to generate code for side
    effects that are meant to take place when the protocol is executed.

  * **Generic Function `PROTOCOL-ELEMENT-BOUNDP`**

    Syntax: `(protocol-element-boundp PROTOCOL-ELEMENT)`

    Checks if the initial value of the protocol element is bound.

    If the protocol element contains an initial value and that value is bound,
    this function returns `(VALUES T T)`.

    If the protocol element contains an initial value and that value is unbound,
    this function returns `(VALUES NIL T)`.

    If the protocol element does not contain an initial value, this function
    returns `(VALUES NIL NIL)`.

  * **Generic Function `PROTOCOL-ELEMENT-MAKUNBOUND`**

    Syntax: `(protocol-element-makunbound PROTOCOL-ELEMENT)`

    Attempts to unbind the initial value of the protocol element.

    If the protocol element contains an initial value and that value is bound,
    this function unbinds that value. Otherwise, it does nothing. In any case,
    the protocol element is returned.

  * **Macro `DEFINE-PROTOCOL`**

    Syntax: `(define-protocol NAME (&rest OPTIONS) &body ELEMENTS)`

    Defines the protocol named `NAME` with the provided `OPTIONS`, containing
    the provided `ELEMENTS`.

    For details, see Options and Protocol Elements below.

  * **Macro `EXECUTE-PROTOCOL`**

    Syntax: `(execute-protocol name)`

    Executes all the side effects of the protocol with the provided `NAME`.

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

## Protocol Elements

TODO accessors for each protocol element class

### :FUNCTION

Describes a generic function that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-FUNCTION`.

The form obeys the following grammar:

```common-lisp
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

If a documentation string is provided, it is attached to the resulting
`DEFGENERIC` form.

### :MACRO

Describes a macro that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-MACRO`.

The form obeys the following grammar:

```common-lisp
(:macro NAME LAMBDA-LIST)
```

  * **`NAME`** - must be a symbol. Denotes the name of the macro.
  * **`LAMBDA-LIST`** - must be a valid macro lambda list.

This element does not expand into anything.

If a documentation string is provided, it is set via `SETF DOCUMENTATION` of
documentation type `FUNCTION`.

### :CLASS

Describes a protocol class that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CLASS`.

The form for obeys the following grammar:

```common-lisp
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

If a documentation string is provided, it is attached to the resulting
`DEFCLASS` form.

### :CONDITION-TYPE

Describes a protocol condition type that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CONDITION-TYPE`.

The form for obeys the following grammar:

```common-lisp
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

If a documentation string is provided, it is attached to the resulting
`DEFINE-CONDITION` form.

### :VARIABLE

Describes a variable that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-VARIABLE`.

The form obeys the following grammar:

```common-lisp
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

If a documentation string is provided, it is set via `SETF DOCUMENTATION` of
documentation type `VARIABLE`.

### :CATEGORY

Describes a configuration category that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CATEGORY`.

The form obeys the following grammar:

```common-lisp
(:category NAME)
```

  * **`NAME`** - must be a non-empty list of keywords. Denotes the name of the
    configuration category. The names of configuration categories and
    configuration entries must not collide with each other.

This element expands into a call to the value of `*CATEGORY-CALLBACK*` with the
category's name being the argument to that function.

If a documentation string is provided, it is set via `SETF DOCUMENTATION` of
documentation type `CATEGORY`.

### :CONFIG

Describes a configuration entry that is a part of a protocol.

These elements are represented by instances of class `PROTOCOL-CONFIG`.

The form obeys the following grammar:

```common-lisp
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

If a documentation string is provided, it is set via `SETF DOCUMENTATION` of
documentation type `CONFIG`.

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

You will need to define methods on generic functions `GENERATE-ELEMENT`,
`GENERATE-FORMS` and `GENERATE-CODE`. For `GENERATE-ELEMENT`, pick a keyword
that will represent your new element and use it in an  EQL specializer when
defining a method on `GENERATE-ELEMENT.`

You may also want to define methods on `DOCUMENTATION` and `SETF DOCUMENTATION`
for your protocol element.

An example PROTEST/PROTOCOL extension with the protocol element `:FOO` with list
representation `(:FOO . FORM)` of class `PROTOCOL-FOO` may look like this:

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
