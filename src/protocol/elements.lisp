;;;; protocol/elements.lisp

(in-package #:protest/protocol)

;;; Protocol functions

(defgeneric name (protocol-element)
  (:documentation
   "Returns the name of the protocol element. The name might be a symbol or a
list of symbols."))

(defgeneric generate-element (type form &optional declaim-type-p)
  (:documentation
   "Given the keyword representing the element type and the rest of that
element's list representation, attempts to generate and return a matching
protocol element. Signals PROTOCOL-ERROR if the generation fails. The argument
DECLAIM-TYPE-P states if the types of functions and variables should be
declaimed; it may be ignored by the method."))

(defgeneric generate-forms (element)
  (:documentation
   "Generates a fresh list of forms that is suitable to be NCONCed with other
forms to generate a protocol body."))

(defgeneric generate-code (object)
  (:documentation
   "Generates a fresh list of forms that is suitable to be NCONCed with other
forms to generate the Lisp code that is meant to come into effect when the
protocol is defined."))

;;; Protocol classes

(define-protocol-class protocol-element ()
  ((%docstring :accessor docstring
               :initarg :docstring
               :initform nil))
  (:documentation "An element of a protocol.
\
This class is a protocol class and must not be instantiated directly."))

(define-protocol-class protocol-operation (protocol-element) ()
  (:documentation "An operation belgonging to a protocol.
\
This class is a protocol class and must not be instantiated directly."))

(define-protocol-class protocol-data-type (protocol-element) ()
  (:documentation "An data type belgonging to a protocol.
\
This class is a protocol class and must not be instantiated directly."))

(defmethod make-load-form ((object protocol-element) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots object))

(defmethod print-object ((object protocol-element) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (name object) stream)))
