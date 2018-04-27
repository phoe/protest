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

  * [`PROTEST/BASE`](doc/base.md) -
    define protocol classes and protocol condition types
  * [`PROTEST/PROTOCOL`](doc/protocolmd) -
    define protocols
  * [`PROTEST/TEST-CASE`](doc/test-case.md) -
    define test cases
  * [`PROTEST/PARACHUTE`](doc/parachutemd) -
    integrate test cases with
    [Parachute](https://github.com/Shinmera/parachute/) testing library

The modules planned for development are:

  * `PROTEST/WEB` -
    output test cases and protocols to HTML
  * `PROTEST/5AM` -
    integrate test cases with [5AM](https://github.com/sionescu/fiveam) testing
    library
  * `PROTEST/PROVE` -
    integrate test cases with [Prove](https://github.com/fukamachi/prove)
    testing library


## Protocol
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

;; TODO exports
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
