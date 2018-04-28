;;;; protocol/elements/config.lisp

(in-package #:protest/protocol)

(defvar *config-callback* (constantly nil)
  "A function of two arguments used as a callback for declaring configuration
values. The first argument is the configuration entry name, the second is the
type that the value of the configuration entry is allowed to take, the third is
a boolean stating if the configuration entry is mandatory to be set, and the
fourth is the initial value that was passed in the protocol. If no optional
value was provided, this argument is not passed.")

(defclass protocol-config (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%value-type :accessor value-type
                :initarg :type
                :initform t)
   (%mandatoryp :accessor mandatoryp
                :initarg :mandatoryp
                :initform nil)
   (%initial-value :accessor initial-value
                   :initarg :initial-value))
  (:documentation
   "Describes a protocol configuration entry that is a part of a protocol.
\
The form for a protocol configuration entry consists of the following subforms:
* NAME - mandatory, must be a list of keywords. Denotes the name of the
  configuration entry. The name of configuration entries and configuration
  categories must not collide with each other.
* VALUE-TYPE - optional, must be a valid type specifier. Denotes the type of the
  value bound to the configuration entry. If not specified, the configuration
  type will default to T.
* MANDATORYP - optional, must be of type (MEMBER :MANDATORY :OPTIONAL). States
  if the configuration entry must have a value set in the configuration before
  any client code may be executed. If not provided, defaults to :OPTIONAL.
* INITIAL-VALUE - optional. Denotes the default value that the configuration
  entry will be bound to at the moment of executing the protocol. If not passed,
  the value will not be bound.")) ;; TODO declaim-type-p

  (defmethod generate-element
      ((type (eql :config)) details &optional declaim-type-p)
    (declare (ignore declaim-type-p))
    (destructuring-bind (name . rest) details
      (declare (ignore rest))
      (assert (every #'keywordp name)
              () "Wrong thing to be a configuration entry name: ~A" name)
      (let ((element (make-instance 'protocol-config :name name)))
        (when (<= 2 (length details))
          (let ((type (second details)))
            (setf (value-type element) type)))
        (when (<= 3 (length details))
          (let ((mandatory (third details)))
            (assert (member mandatory '(:mandatory :optional))
                    () "~S must be one of :MANDATORY :OPTIONAL." mandatory)
            (setf (mandatoryp element) (eq mandatory :mandatory))))
        (when (<= 4 (length details))
          (let ((initial-value (fourth details)))
            (setf (initial-value element) initial-value)))
        element)))

(defmethod generate-forms ((element protocol-config))
  (let* ((name (name element)) (type (value-type element))
         (mandatoryp (mandatoryp element))
         (documentation (docstring element)))
    `((:config ,name
               ,@(cond ((and (eq type 't) (eq mandatoryp 'nil)
                             (not (slot-boundp element '%initial-value)))
                        '())
                       ((and (eq mandatoryp 'nil)
                             (not (slot-boundp element '%initial-value)))
                        `(,type))
                       ((not (slot-boundp element '%initial-value))
                        `(,type ,(if mandatoryp :mandatory :optional)))
                       (t `(,type ,(if mandatoryp :mandatory :optional)
                                  ,(initial-value element)))))
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-config))
  (let ((documentation (docstring element)))
    `(,@(when (slot-boundp element '%initial-value)
          `((funcall *config-callback*
                     ',(name element)
                     ',(value-type element)
                     ,(mandatoryp element)
                     ,@(when (slot-boundp element '%initial-value)
                         (list (initial-value element))))))
      ,@(when documentation
          `((setf (documentation ',(name element) 'config) ,documentation))))))

(defvar *config-documentation-store*
  (make-hash-table :test #'equal))

(defmethod documentation ((slotd list) (doc-type (eql 'config)))
  (values (gethash slotd *config-documentation-store*)))

(defmethod (setf documentation)
    (new-value (slotd list) (doc-type (eql 'config)))
  (if new-value
      (setf (gethash slotd *config-documentation-store*) new-value)
      (remhash slotd *config-documentation-store*))
  new-value)

(defmethod protocol-element-boundp ((element protocol-config))
  (if (slot-boundp element '%initial-value)
      (values t t)
      (values nil t)))

(defmethod protocol-element-makunbound ((element protocol-config))
  (when (slot-boundp element '%initial-value)
    (slot-makunbound element '%initial-value))
  element)
