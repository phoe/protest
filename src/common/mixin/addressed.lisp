;;;; src/common/addressed.lisp

(defpackage #:protest/common/addressed
  (:use #:common-lisp)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))

(in-package #:protest/common/addressed)

(define-protocol addressed
    (:documentation "The ADDRESSED protocol describes objects which have at ~
least one network address, consisting of a hostname and a port. That address ~
may be exported as a human-readable string.
\
All generic functions specified in this protocol specify an optional TYPE ~
argument. This argument may be used to differentiate between different types ~
of network addresses. Methods specified on these generic functions may specify ~
a default type on the keyword.
\
A default method is provided on the ADDRESS function that concatenates the ~
hostname and the port with a #\: character in between. Classes are free to ~
implement this method in subclasses."
     :tags (:addressed)
     :export t)
  (:class addressed () ())
  "An addressed object. See protocol ADDRESSED for details."
  (:function hostname ((addressed addressed) &optional type) string)
  "Returns the addressed object's hostname."
  (:function port ((addressed addressed) &optional type) unsigned-byte)
  "Returns the addressed object's port."
  (:function address ((addressed addressed) &optional type) string)
  "Returns the address of the addressed object socket in a string form for ~
textual representation.")

(execute-protocol addressed)

(defmethod address ((addressed addressed) &optional type)
  (format nil "~A:~A" (hostname addressed type) (port addressed type)))
