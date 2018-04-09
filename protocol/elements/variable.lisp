;;;; protocol/elements/variable.lisp

(in-package #:protest/protocol)

(defclass protocol-variable (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%type :accessor type
          :initarg :type
          :initform t)
   (%initial-value :accessor initial-value
                   :initarg :initial-value))
  (:documentation
   "Describes a protocol variable that is a part of a protocol.
\
The form for a protocol variable consists of the following subforms:
* NAME - mandatory, must be a symbol. Denotes the name of the variable.
* TYPE - optional, must be a valid type specifier. Denotes the type of the value
  bound to the variable. If not passed, the variable type will not be declaimed.
* INITIAL-VALUE - optional. Denotes the default value that the variable will
  have at the moment of defining the protocol. If not passed, the variable will
  be unbound."))

(defmethod generate-element ((type (eql :variable)) &rest form)
  (destructuring-bind (name . rest) form
    (declare (ignore rest))
    (assert (and (not (null name)) (symbolp name))
            () "Wrong thing to be a variable name: ~S" name)
    (let ((element (make-instance 'protocol-variable :name name)))
      (when (<= 2 (length form))
        (let ((type (second form)))
          (setf (type element) type)))
      (when (<= 3 (length form))
        (let ((initial-value (third form)))
          (setf (initial-value element) initial-value)))
      element)))

(defmethod embed-documentation ((element protocol-variable) (string string))
  (setf (documentation (name element) 'variable) string))

(defmethod generate-forms ((element protocol-variable))
  (let* ((name (name element))
         (type (type element))
         (documentation (documentation name 'variable)))
    `((:variable ,name ,@(unless (eq type 't) `(,type))
                 ,@(when (slot-boundp element '%initial-value)
                     `(,(initial-value element))))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-variable))
  (with-accessors
        ((name name) (type type) (initial-value initial-value))
      element
    (let ((documentation (documentation name 'variable)))
      `(,@(when (and *declaim-types* (not (eq type 't)))
            `((declaim (cl:type ,type ,name))))
        (defvar ,name ,@(when (slot-boundp element '%initial-value)
                          `(,(initial-value element))))
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))))))
