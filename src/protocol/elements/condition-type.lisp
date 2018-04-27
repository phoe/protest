;;;; protocol/elements/condition-type.lisp

(in-package #:protest/protocol)

(defclass protocol-condition-type (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%supertypes :accessor supertypes
                :initarg :supertypes
                :initform '())
   (%slots :accessor slots
           :initarg :slots
           :initform '())
   (%options :accessor options
             :initarg :options
             :initform '()))
  (:documentation
   "Describes a protocol condition type that is a part of a protocol.
\
The form for a protocol condition type consists of the following subforms:
* NAME - mandatory, must be a symbol. Denotes the name of the condition type.
* SUPERTYPES - mandatory, must be a list of symbols. Denotes the supertypes
  of the condition type.
* SLOTS - mandatory, must be a list of slot definitions. Denotes the slots of
  the condition type. It is discouraged to create slots in protocol condition
  types; client code should instead create slots in concrete types which
  subtype the protocol condition types.
* OPTIONS - optional, is the tail of the list. Denotes the options that will
  be passed to DEFINE-CONDITION."))

(defmethod generate-element
    ((type (eql :condition-type)) form &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name supertypes slots . options) form
    (assert (and (not (null name)) (symbolp name))
            () "Wrong thing to be a condition type name: ~S" name)
    (assert (every #'symbolp supertypes)
            () "Incorrect supertype list: ~S" supertypes)
    (let ((element (make-instance 'protocol-condition-type
                                  :name name :supertypes supertypes
                                  :slots slots :options options)))
      element)))

(defmethod generate-forms ((element protocol-condition-type))
  (let* ((name (name element))
         (documentation (docstring element)))
    `((:condition-type ,name ,(supertypes element)
                       ,(slots element) ,@(options element))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-condition-type))
  (with-accessors
        ((name name) (supertypes supertypes) (slots slots) (options options))
      element
    (let ((documentation (docstring element)))
      `((define-protocol-condition-type ,name ,supertypes ,slots ,@options
          ,@(when documentation `((:documentation ,documentation))))))))
