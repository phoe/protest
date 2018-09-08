;;;; src/common/handling.lisp

(in-package #:protest/common)

(define-protocol handling
    (:documentation "The HANDLING protocol describes objects which have a
handler function.
\
This protocol specifies only a mechanism of accessing the handler function and
does not concern itself with the arity or arguments of the handler function
itself."
     :tags (:handling)
     :export t)
  (:class handling () ())
  "An object with a handler. See protocol HANDLING for details."
  (:function handler ((object handling)) function)
  "Returns the handler function of the object."
  (:function (setf handler) (new-value (object handling)) t)
  "Sets the handler function of the object.")

(execute-protocol handling)
