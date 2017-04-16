![CL-PROTEST](/logo.png)
# Common Lisp PROtocol and TESTcase manager

This is heavily WIP.

## TODO
  * Fix and document DEFINE-TEST-CASE
  * Integrate DEFINE-TEST-CASE with some testing framework
  * Unit tests for macros and functions

## Usage
The code below:

```lisp
;;;; Example test case and protocol.

(define-protocol wiggler ()
  (:class wiggler () ())
  "An object that is able to wiggle."
  (:variable *wiggler* t nil)
  "A dynamic variable denoting the current active wiggler."
  (:macro with-wiggler)
  "A wrapper macro that binds *WIGGLER* to the value of WIGGLER."
  (:function make-wiggler ((type (or class symbol))) (wiggler wiggler))
  "A constructor function that makes a wiggler of given type."
  (:generic wiggle ((wiggler wiggler) (object fist)) (values))
  "Wiggles inside target object.")

(define-protocol killable ()
  (:class killable () ())
  "A killable object is something that lives and can therefore be killed."
  (:generic alivep ((object killable)) :generalized-boolean)
  "Returns true if the object is alive (was not killed) and false otherwise."
  (:generic kill ((object killable)) (values))
  "Kills the object.")

(define-protocol fist ()
  (:class fist (killable) ())
  "A fist is something that can squeeze around objects and hold them ~
despite any wiggling. If a fist dies, then it is possible to wiggle ~
out of it."
  (:generic squeeze ((object fist)) :generalized-boolean)
  "Squeezes the fist.
Returns true if the first was not previously squeezed and false otherwise."
  (:generic unsqueeze ((object fist)) :generalized-boolean)
  "Unsqueezes the fist.
Returns true if the first was previously squeezed and false otherwise."
  (:generic wigglep ((object fist)) :generalized-boolean)
  "Checks if anything wiggled inside the fist since its last squeeze.")
```

Produces the following effects:

### Doc Generation
The package `CL-PROTEST-WEB` contains the functions to produce HTML documentation of the protocols - see https://rawgit.com/phoe/cl-protest/master/example.html .

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
