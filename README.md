<p align="center">
  <img src="logo.png">
</p>

# Common Lisp PROtocol and TESTcase manager

PROTEST is a tool for defining protocols and test cases.

For a formal definition of a protocol, see the related work by
[Robert Strandh](http://metamodular.com/protocol.pdf).

## Protocol

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

Each phase is required to be a keyword. These are meant to describe the group of
test steps that come afterwards it.

Each step is required to consist of a positive integer and a string
description of the step. The steps are required to be unsigned-bytes in
increasing order.

Inside a test body, the reader macro `#N?` with a numerical argument may be
used to denote a form belonging to a given test step.

## TODO
  * Implement :DEPENDENCIES in DEFINE-PROTOCOL.
  * Take care of STYLE-WARNINGs when defining :FUNCTIONs.
  * Ordinary functions are no longer creatable here - :GENERIC is removed and
  instead, :FUNCTION creates a generic function.
  * Fix and document DEFINE-TEST-CASE
  * Integrate DEFINE-TEST-CASE with some testing framework
  * Unit tests for macros and functions

## Usage
The code below:

```common-lisp
(define-protocol fuelable
    (:description "Defines objects which have a fuel tank and must be refueled
to function."
     :tags (:industrial :electricity :coal :oil)
     :export t)
  (:function fuel ((object fuelable)) real)
  "Returns the current amount of fuel in the fuelable."
  (:function (setf fuel) ((new-value real) (object-fuelable)) real)
  "Sets the current amount of fuel in the fuelable.")

(define-protocol car
    (:description "Defines objects which are able to move forward by means of an
engine and can hold people and luggage."
     :tags (:industrial :machine)
     :dependencies (fuelable)
     :export t)
  (:class car (fuelable) ())
  "A car object. Objects participaring in this protocol must subclass this
protocol class."
  (:function brand ((object car)) keyword)
  "Returns the brand of a car."
  (:function (setf brand) ((new-value keyword) (object car)) keyword)
  "Sets the brand of a car."
  (:function drive ((object car) (distance real) &optional env) (values))
  "Drives a given distance with the given car. If ENV is supplied, the drive
occurs in that environment."
  (:macro with-snow-tires ((tire-brand) &body body))
  "Executes the body with snow tires on all cars, therefore diminishing chances
of an accident in snow environment."
  (:variable *accident-chance* (float 0.0 1.0) 0.005)
  "The chance of an accident per 100 kilometers. This variable is expected to
be rebound when the environment changes."
  (:category :car)
  "Describes configuration entries related to all cars in general."
  (:config (:car :maximum-lead-per-100-kilometers) (float 0.0) :optional 0.0005)
  "Describes how many grams of lead an engine is allowed to output after driving
for 100 kilometers."
  (:category :car :steering)
  "Describes configuration entries related to the cars' steering."
  (:config (:car :steering :wheel-side) (member :left :right :any) :mandatory)
  "Describes if the cars must have steering wheels on left or right side.")
  ```

Produces the following effects:

### Doc Generation
The package `CL-PROTEST-WEB` contains the functions to produce HTML documentation of the protocols - see https://rawgit.com/phoe/protest/master/doc/example.html .

### Code Generation
  * All DEFINE-PROTOCOL forms (sans documentation strings) are stored in the variable `*PROTOCOLS*`.
  * All documentation strings are stored in proper parts of the `CL:DOCUMENTATION` system.
  * Checks are made to make sure that protocol classes and generic functions are defined, but never _re_defined.
  * A code equivalent to the following is executed:

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Protocol WIGGLER
;; Protocol Class WIGGLER
(DEFINE-PROTOCOL-CLASS WIGGLER NIL NIL)
(SETF (DOCUMENTATION 'WIGGLER 'TYPE) "An object that is able to wiggle.")
;; Variable *WIGGLER*
(SETF (DOCUMENTATION '*WIGGLER* 'VARIABLE)
      "A dynamic variable denoting the current active wiggler.")
(DECLAIM (TYPE T *WIGGLER*))
(DEFVAR *WIGGLER* NIL)
;; Macro WITH-WIGGLER
(SETF (DOCUMENTATION 'WITH-WIGGLER 'VARIABLE)
      "A wrapper macro that binds *WIGGLER* to the value of WIGGLER.")
;; Function MAKE-WIGGLER
(SETF (DOCUMENTATION 'MAKE-WIGGLER 'FUNCTION)
      "A constructor function that makes a wiggler of given type.")
(DECLAIM (FTYPE (FUNCTION ((OR CLASS SYMBOL))) MAKE-WIGGLER))
;; Generic Function WIGGLE
(SETF (DOCUMENTATION 'WIGGLE 'FUNCTION) "Wiggles inside target object.")
(DEFGENERIC WIGGLE (WIGGLER OBJECT))
(DECLAIM (FTYPE (FUNCTION * (VALUES)) WIGGLE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Protocol KILLABLE
;; Protocol Class KILLABLE
(DEFINE-PROTOCOL-CLASS KILLABLE NIL NIL)
(SETF (DOCUMENTATION 'KILLABLE 'TYPE)
      "A killable object is something that lives and can therefore be killed.")
;; Generic Function ALIVEP
(SETF (DOCUMENTATION 'ALIVEP 'FUNCTION)
      "Returns true if the object is alive (was not killed) and false otherwise.")
(DEFGENERIC ALIVEP (OBJECT))
(DECLAIM (FTYPE (FUNCTION * T) ALIVEP))
;; Generic Function KILL
(SETF (DOCUMENTATION 'KILL 'FUNCTION) "Kills the object.")
(DEFGENERIC KILL (OBJECT))
(DECLAIM (FTYPE (FUNCTION * (VALUES)) KILL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Protocol FIST
;; Protocol Class FIST
(DEFINE-PROTOCOL-CLASS FIST (KILLABLE) NIL)
(SETF (DOCUMENTATION 'FIST 'TYPE)
      "A fist is something that can squeeze around objects and hold them despite any wiggling. If a fist dies, then it is possible to wiggle out of it.")
;; Generic Function SQUEEZE
(SETF (DOCUMENTATION 'SQUEEZE 'FUNCTION) "Squeezes the fist.
Returns true if the first was not previously squeezed and false otherwise.")
(DEFGENERIC SQUEEZE (OBJECT))
(DECLAIM (FTYPE (FUNCTION * T) SQUEEZE))
;; Generic Function UNSQUEEZE
(SETF (DOCUMENTATION 'UNSQUEEZE 'FUNCTION) "Unsqueezes the fist.
Returns true if the first was previously squeezed and false otherwise.")
(DEFGENERIC UNSQUEEZE (OBJECT))
(DECLAIM (FTYPE (FUNCTION * T) UNSQUEEZE))
;; Generic Function WIGGLEP
(SETF (DOCUMENTATION 'WIGGLEP 'FUNCTION)
      "Checks if anything wiggled inside the fist since its last squeeze.")
(DEFGENERIC WIGGLEP (OBJECT))
(DECLAIM (FTYPE (FUNCTION * T) WIGGLEP))
```
