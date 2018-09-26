;;;; src/common/handling.lisp

(defpackage #:protest/common/handling
  (:use #:common-lisp)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))

(in-package #:protest/common/handling)

(define-protocol handling
    (:documentation "The HANDLING protocol describes objects which have a
handler function.
\
All generic functions specified in this protocol specify an optional TYPE ~
argument. This argument may be used to differentiate between different types ~
of network addresses. Methods specified on these generic functions may specify ~
a default type on the keyword.
\
This protocol specifies only a mechanism of accessing the handler function and
does not concern itself with the arity or arguments of the handler function
itself."
     :tags (:handling)
     :export t)
  (:class handling () ())
  "An object with a handler. See protocol HANDLING for details."
  (:function handler ((object handling) &optional type) function)
  "Returns the handler function of the object."
  (:function (setf handler) (new-value (object handling) &optional type) t)
  "Sets the handler function of the object.")

(execute-protocol handling)
