;;;; src/common/named.lisp

(defpackage #:protest/common/named
  (:use #:common-lisp
        #:protest/common)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))

(in-package #:protest/common/named)

(define-protocol named
    (:documentation "The NAMED protocol describes objects which have a name - ~
a human-readable string that denotes that object's identity, along with other ~
attributes of that object."
     :tags (:named)
     :export t)
  (:class named () ())
  "A named object. See protocol NAMED for details."
  (:function name ((object named)) string)
  "Returns the name of the target object.")

(execute-protocol named)
