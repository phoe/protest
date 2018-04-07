;;;; protocol/elements.lisp

(in-package #:protest/protocol)

(defgeneric generate-element (type &rest form)
  (:documentation
   "Given the element type and the rest of the form, attempts to generate and
return a matching protocol element. Signals PROTOCOL-ERROR if the generation
fails."))

(defgeneric embed-documentation (element string)
  (:documentation
   "Given a protocol element and a documentation string, sets the documentation
string of the given element."))

(defgeneric generate-forms (element)
  (:documentation
   "Generates a fresh list of forms that is suitable to be NCONCed with other
forms to generate a protocol body."))

(define-protocol-class protocol-element () ()
  (:documentation "An element of a protocol.
\
This class is a protocol class and must not be instantiated directly."))

(defgeneric name (protocol-element))

(defmethod print-object ((object protocol-element) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (name object) stream)))

(define-protocol-class protocol-operation () ()
  (:documentation "An operation belgonging to a protocol.
\
This class is a protocol class and must not be instantiated directly."))

(define-protocol-class protocol-data-type () ()
  (:documentation "An data type belgonging to a protocol.
\
This class is a protocol class and must not be instantiated directly."))
