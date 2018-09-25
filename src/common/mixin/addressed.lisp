;;;; src/common/addressed.lisp

(defpackage #:protest/common/addressed
  (:use #:common-lisp)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))

(in-package #:protest/common/addressed)

(define-protocol addressed
    (:documentation "The ADDRESSED protocol describes objects which have a ~
network address, consisting of a hostname and a port. That address may be ~
exported as a human-readable string.
\
A default method is provided on the ADDRESS function that concatenates the ~
hostname and the port with a #\: character in between. Classes are free to ~
implement this method in subclasses."
     :tags (:addressed)
     :export t)
  (:class addressed () ())
  "An addressed object. See protocol ADDRESSED for details."
  (:function hostname ((addressed addressed)) string)
  "Returns the addressed object's hostname."
  (:function port ((addressed addressed)) unsigned-byte)
  "Returns the addressed object's port."
  (:function address ((addressed addressed)) string)
  "Returns the address of the addressed object socket in a string form for ~
textual representation.")

(execute-protocol addressed)

(defmethod address ((addressed addressed))
  (format nil "~A:~A" (hostname addressed) (port addressed)))
