;;;; src/protocol/elements.lisp

(in-package #:protest/protocol)

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

;;; Protocol functions

(defgeneric name (protocol-element)
  (:documentation
   "Returns the name of the protocol element. The name might be a symbol or a
list of symbols."))

(defgeneric keyword-element-class (keyword)
  (:documentation
   "Given the keyword representing the element type, returns the class object
representing the concrete element type."))

(defgeneric generate-element-using-class
    (class details &optional declaim-type-p)
  (:documentation
   "Given the concrete class of the element type and the list representing that
element's list representation, attempts to generate and return a matching
protocol element. Signals PROTOCOL-ERROR if the generation fails. The argument
DECLAIM-TYPE-P states if the types of functions and variables should be
declaimed; it may be ignored by the method."))

(defun generate-element (type details &optional declaim-type-p)
  "Given the keyword representing the element type and the rest of that
element's list representation, attempts to generate and return a matching
protocol element. Signals PROTOCOL-ERROR if the generation fails. The argument
DECLAIM-TYPE-P states if the types of functions and variables should be
declaimed; it may be ignored by the method."
  (let ((class (keyword-element-class type)))
    (generate-element-using-class class details declaim-type-p)))

(defgeneric generate-forms (element)
  (:documentation
   "Generates a fresh list of forms that is suitable to be NCONCed with other
forms to generate a protocol body."))

(defgeneric generate-code (object)
  (:documentation
   "Generates a fresh list of forms that is suitable to be NCONCed with other
forms to generate the Lisp code that is meant to come into effect when the
protocol is defined."))

(defgeneric protocol-element-boundp (protocol-element)
  (:documentation
   "Checks if the initial value of the protocol element is bound.
\
If the protocol element contains an initial value and that value is bound,
this function returns (VALUES T T).
If the protocol element contains an initial value and that value is unbound,
this function returns (VALUES NIL T).
If the protocol element does not contain an initial value, this function returns
\(VALUES NIL NIL).")
  (:method ((protocol-element protocol-element))
    (values nil nil)))

(defgeneric protocol-element-makunbound (protocol-element)
  (:documentation
   "Attempts to unbind the initial value of the protocol element.
\
If the protocol element contains an initial value and that value is bound,
this function unbinds that value. Otherwise, it does nothing.
In any case, the protocol element is returned.")
  (:method ((protocol-element protocol-element))
    protocol-element))

(defgeneric canonical-name (protocol-element)
  (:documentation "Returns the canonical name of the protocol element.
\
If a protocol element has a name that is a list, a canonical name of that
element is a copy of that element's name, in which non-keyword symbols have
been replaced by NIL. Otherwise, the canonical name is the name of the
element.")
  (:method ((protocol-element protocol-element))
    (name protocol-element)))

(defgeneric remove-protocol-element (protocol-element)
  (:documentation "Removes the effects of the provided protocol element from the
Lisp image.
\
This function does nothing unless the protocol associated with this element has
been executed.")
  (:method ((protocol-element protocol-element))))
