;;;; src/protocol/elements/class.lisp

(in-package #:protest/protocol)

(defclass protocol-class (protocol-data-type)
  ((%name :reader name
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

(defmethod keyword-element-class ((keyword (eql :class)))
  (find-class 'protocol-class))

(defmethod generate-element-using-class
    ((class (eql (find-class 'protocol-class))) details &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name superclasses slots . options) details
    (assert (and (not (null name)) (symbolp name))
            () "Wrong thing to be a class name: ~S" name)
    (assert (every #'symbolp superclasses)
            () "Incorrect superclass list: ~S" superclasses)
    (let ((element (make-instance class
                                  :name name :superclasses superclasses
                                  :slots slots :options options)))
      element)))

(defmethod generate-forms ((element protocol-class))
  (let* ((name (name element))
         (documentation (docstring element)))
    `((:class ,name ,(superclasses element)
              ,(slots element) ,@(options element))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-class))
  (with-accessors
        ((name name) (superclasses superclasses)
         (slots slots) (options options))
      element
    (let ((documentation (docstring element)))
      `((define-protocol-class ,name ,superclasses ,slots ,@options
          ,@(when documentation `((:documentation ,documentation))))))))

(defmethod remove-protocol-element ((element protocol-class))
  (let ((name (name element)))
    (setf (documentation name 'type) nil)
    (remove-protocol-object (find-class name))
    (setf (find-class name) nil)))
