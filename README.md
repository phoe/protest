<p align="center">
  <img src="logo.png">
</p>

# Common Lisp PROtocol and TESTcase manager

PROTEST is a tool for defining protocols and test cases.

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

TODO

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
