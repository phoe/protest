;;;; protocol/elements/macro.lisp

(in-package #:protest/protocol)

(defclass protocol-macro (protocol-operation)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%lambda-list :accessor lambda-list
                 :initarg :lambda-list
                 :initform (error "Must provide LAMBDA-LIST.")))
  (:documentation
   "Describes a protocol macro that is a part of a protocol."))

(defmethod generate-element ((type (eql :macro)) &rest form)
  (error "not implemented"))
