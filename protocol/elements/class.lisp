;;;; protocol/elements/class.lisp

(in-package #:protest/protocol)

(defclass protocol-class (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%superclasses :accessor superclasses
                  :initarg :superclasses
                  :initform '())
   (%slots :accessor slots
           :initarg :slots
           :initform '())
   (%options :accessor options
             :initarg :options
             :initform '()))
  (:documentation
   "Describes a protocol class that is a part of a protocol.
\
The form for a protocol class consists of the following subforms:
* NAME - mandatory, must be a symbol. Denotes the name of the class.
* SUPERCLASSES - mandatory, must be a list of symbols. Denotes the superclasses
  of the class.
* SLOTS - mandatory, must be a list of slot definitions. Denotes the slots of
  the class. It is discouraged to create slots in protocol classes; client code
  should instead create slots in concrete classes which subclass the protocol
  classes.
* OPTIONS - optional, is the tail of the list. Denotes the options that will
  be passed to DEFCLASS."))

(defmethod generate-element ((type (eql :class)) form &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name superclasses slots . options) form
    (assert (and (not (null name)) (symbolp name))
            () "Wrong thing to be a class name: ~S" name)
    (assert (every #'symbolp superclasses)
            () "Incorrect superclass list: ~S" superclasses)
    (let ((element (make-instance 'protocol-class
                                  :name name :superclasses superclasses
                                  :slots slots :options options)))
      element)))

(defmethod embed-documentation ((element protocol-class) (string string))
  (setf (documentation (name element) 'cl:type) string))

(defmethod generate-forms ((element protocol-class))
  (let* ((name (name element))
         (documentation (documentation name 'cl:type)))
    `((:class ,name ,(superclasses element)
              ,(slots element) ,@(options element))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-class))
  (with-accessors
        ((name name) (superclasses superclasses)
         (slots slots) (options options))
      element
    (let ((documentation (documentation name 'cl:type)))
      `((define-protocol-class ,name ,superclasses ,slots ,@options
          ,@(when documentation `((:documentation ,documentation))))))))
