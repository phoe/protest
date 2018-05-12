;;;; src/protocol/elements/variable.lisp

(in-package #:protest/protocol)

(defclass protocol-variable (protocol-data-type)
  ((%name :reader name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%value-type :accessor value-type
                :initarg :value-type
                :initform t)
   (%initial-value :accessor initial-value
                   :initarg :initial-value)
   (%declaim-type-p :accessor declaim-type-p
                    :initform t))
  (:documentation
   "Describes a protocol variable that is a part of a protocol.
\
The form for a protocol variable consists of the following subforms:
* NAME - mandatory, must be a symbol. Denotes the name of the variable.
* VALUE-TYPE - optional, must be a valid type specifier. Denotes the type of
  the value bound to the variable. If not passed, the variable type will not be
  declaimed.
* INITIAL-VALUE - optional. Denotes the default value that the variable will be
  bound to at the moment of executing the protocol. If not passed, the variable
  will be unbound."))

(defmethod generate-element
    ((type (eql :variable)) details &optional (declaim-type-p t))
  (destructuring-bind (name . rest) details
    (declare (ignore rest))
    (assert (and (not (null name)) (symbolp name)) ()
            "Wrong thing to be a variable name: ~S" name)
    (let ((element (make-instance 'protocol-variable :name name)))
      (setf (declaim-type-p element) declaim-type-p)
      (when (<= 2 (length details))
        (let ((type (second details)))
          (setf (value-type element) type)))
      (when (<= 3 (length details))
        (let ((initial-value (third details)))
          (unless (typep initial-value (second details))
            (protocol-error "The provided initial value, ~S, is not of the ~
provided type ~S." (value-type element)) initial-value)
          (setf (initial-value element) initial-value)))
      element)))

(defmethod embed-documentation ((element protocol-variable) (string string))
  (setf (documentation (name element) 'variable) string))

(defmethod generate-forms ((element protocol-variable))
  (let* ((name (name element))
         (type (value-type element))
         (documentation (docstring element)))
    `((:variable ,name ,@(unless (eq type 't) `(,type))
                 ,@(when (slot-boundp element '%initial-value)
                     `(,(initial-value element))))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-variable))
  (with-accessors
        ((name name) (value-type value-type)
         (initial-value initial-value))
      element
    (let ((documentation (docstring element)))
      `((defvar ,name ,@(when (slot-boundp element '%initial-value)
                          `(,(initial-value element))))
        ,@(when (and (declaim-type-p element) (not (eq value-type 't)))
            `((declaim (type ,value-type ,name))))
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))))))

(defmethod protocol-element-boundp ((element protocol-variable))
  (if (slot-boundp element '%initial-value)
      (values t t)
      (values nil t)))

(defmethod protocol-element-makunbound ((element protocol-variable))
  (when (slot-boundp element '%initial-value)
    (slot-makunbound element '%initial-value))
  element)
