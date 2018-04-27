<p align="center">
  <img src="doc/logo.png">
</p>

# Common Lisp PROtocol and TESTcase manager

PROTEST is a tool for defining protocols and test cases written in and for
Common Lisp.

## tl;dr

TODO

## Modules

The currently implemented modules are:

  * [`PROTEST/BASE`](src/base/README.md) - define protocol classes and protocol condition types
  * [`PROTEST/PROTOCOL`](src/protocol/README.md) - define protocols
  * [`PROTEST/TEST-CASE`](src/test-case/README.md) - define test cases
  * [`PROTEST/PARACHUTE`](src/parachute/README.md) - integrate test cases with Parachute testing library

The modules planned for development are:

  * `PROTEST/WEB` - output test cases and protocols to HTML
  * `PROTEST/5AM` - integrate test cases with 5AM testing library
  * `PROTEST/PROVE` - integrate test cases with Prove testing library

## Base

**Summary:** The package `PROTEST/BASE` exports the base symbols useful for
manual definition of protcols. The macros `DEFINE-PROTOCOL-CLASS` and
`DEFINE-PROTOCOL-CONDITION-TYPE` can be used for defining protocol classes and
protocol condition types, while condition type `PROTOCOL-OBJECT-INSTANTIATION`
can be handled specifically to handle errors that arise when users attempt to
instantiate these protocol objects. Additionally, the condition type
`PROTOCOL-ERROR` can be handled to handle all errors that arise regarding
protocol and test-case definition. This condition type is later reused
in other PROTEST packages.

**Internal dependencies:** none. This package can be used standalone.

### Exported symbols:

  * **Macro `DEFINE-PROTOCOL-CLASS`**

    Like `DEFCLASS`, but instances of the defined class will not be directly
    instantiable.

  * **Macro `DEFINE-PROTOCOL-CONDITION-TYPE`**

    Like `DEFINE-CONDITION`, but instances of the defined condition type will
    not be directly instantiable.

    **SBCL Note:** This constraint is not enforced on SBCL. See
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

## Protocol

For a formal definition of a protocol, see the related work by
[Robert Strandh](http://metamodular.com/protocol.pdf).

PROTEST implements the concept of a protocol with operations being defined as
generic functions and macros, and data types being protocol classes, protocol
condition types, and special variables.

Protocol classes and protocol condition types are instances of Common Lisp
classes and condition types, respectively, except they must not be instantiated
directly by client code. Users of these classes must instead create subclasses
of these protocol classes or subtypes of these protocol condition types in order
for them to participate in the protocol. For utility, PROTEST also describes
configuration categories and entries, defined as list of keywords and
non-keyword symbols, along with the value that each configuration entry is
allowed to take.

A protocol in PROTEST consists of metadata about a given protocol, such as
description and tags, and a series of protocol element definitions.

Each element of the protocol is described by a form whose first element is the
keyword denoting the type of a given element.

## Test case

PROTEST implements the concept of a test case as an object describing the flow
of a test.

A test case in PROTEST consists of three elements: metadata about a
given test phase, such as description and tags, test phases (denoted by
keywords), and test steps (denoted by numbers and strings). PROTEST also
implements the concept of a test that is an instance of the given test case.

Each test case in PROTEST has a name, which may be a string or a symbol. All names
are internally coerced to strings.

Each phase is required to be a keyword. These are meant to describe the group of
test steps that come afterwards it.

Each step is required to consist of a positive integer and a string
description of the step. The steps are required to be unsigned-bytes in
increasing order.

Inside a test body, the reader macro `#N?` with a numerical argument may be
used to denote a form belonging to a given test step.

## HTML generation

TODO

## Integration with Parachute

TODO

## Usage
The code below:

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
  (:class automobile (fuelable) ())
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
  (:config (:automobile :maximum-lead-per-100-kilometers) (float 0.0) :optional 0.0005)
  "Describes how many grams of lead an engine is allowed to output after driving
for 100 kilometers."
  (:category (:automobile :steering))
  "Describes configuration entries related to the automobiles' steering."
  (:config (:automobile :steering :wheel-side) (member :left :right :any)
           :mandatory)
  "Describes if the automobiles must have steering wheels on left or right side.")
  ```

Produces the following effects:

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

;;;; Protocol AUTOMOBILE
(setf (documentation automobile 'protocol)
      "Defines objects which are able to move forward by means of an
engine and can hold people and luggage.")

(define-protocol-class automobile (fuelable) nil
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
         '(:automobile :maximum-lead-per-100-kilometers) 5.0e-4)
(setf (documentation '(:automobile :maximum-lead-per-100-kilometers) 'config)
      "Describes how many grams of lead an engine is allowed to output after
driving for 100 kilometers.")

(setf (documentation '(:automobile :steering) 'category)
      "Describes configuration entries related to the automobiles' steering.")

(setf (documentation '(:automobile :steering :wheel-side) 'config)
      "Describes if the automobiles must have steering wheels on left or right
side.")
```

### License

All of PROTEST is licensed under GNU Lisp Lesser General Public License,
**except** the `PROTEST/PARACHUTE` system which is an extension to the Parachute
library and therefore licensed under the Artistic license.

-----------

PROTEST © 2018 Michał "phoe" Herda

This library is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 2.1 of the License, or (at your option) any
later version.

This library is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this library; if not, write to the Free Software Foundation, Inc., 51
Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
