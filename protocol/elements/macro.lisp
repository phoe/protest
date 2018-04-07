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
   "Describes a protocol macro that is a part of a protocol.
\
The form of a protocol macro consists of the following subforms:
* NAME - mandatory, must be a symbol. Denotes the name of the macro.
* LAMBDA-LIST - mandatory, must be a valid macro lambda list."))

(defmethod generate-element ((type (eql :macro)) &rest form)
  (destructuring-bind (name lambda-list . rest) form
    (declare (ignore rest))
    (assert (and (not (null name)) (symbolp name))
            () "Wrong thing to be a macro name: ~S" name)
    (let ((element (make-instance 'protocol-macro :name name
                                                  :lambda-list lambda-list)))
      element)))

(defmethod embed-documentation ((element protocol-macro) (string string))
  (setf (documentation (name element) 'function) string))

(defmethod generate-forms ((element protocol-macro))
  (let* ((name (name element))
         (documentation (documentation name 'function)))
    `((:macro ,name ,(lambda-list element))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-macro))
  '())
