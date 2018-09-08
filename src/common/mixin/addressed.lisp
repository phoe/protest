;;;; src/common/addressed.lisp

(in-package #:protest/common)

(define-protocol addressed
    (:documentation "The ADDRESSED protocol describes objects which have a ~
network address. That address may be exported as a human-readable string."
     :tags (:addressed)
     :export t)
  (:class addressed () ())
  "An addressed object. See protocol ADDRESSED for details."
  (:function address ((addressed addressed)) string)
  "Returns the address of the addressed object socket in a string form.")

(execute-protocol addressed)
