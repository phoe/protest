;;;; protocol/elements/config.lisp

(in-package #:protest/protocol)

(defvar *configuration-setter* (constantly nil)
  "A function of two arguments used for setting configuration values. The first
argument is the configuration entry name and the second is the value that should
be set to it.")

(defvar *compile-time-known-protocols*)

(defclass protocol-config (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%type :accessor type
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
* TYPE - optional, must be a valid type specifier. Denotes the type of the value
  bound to the configuration entry. If not specified, the configuration type
  will default to T.
* MANDATORYP - optional, must be of type (MEMBER :MANDATORY :OPTIONAL). States
  if the configuration entry must have a value set in the configuration before
  any client code may be executed. If not provided, defaults to :OPTIONAL.
* INITIAL-VALUE - optional. Denotes the default value that the configuration
  entry should have at the moment of defining the protocol. If not passed,
  the value will not be set."))

(defmethod generate-element ((type (eql :config)) form &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name . rest) form
    (declare (ignore rest))
    (assert (every #'keywordp name)
            () "Wrong thing to be a configuration entry name: ~A" name)
    (let ((element (make-instance 'protocol-config :name name)))
      (when (<= 2 (length form))
        (let ((type (second form)))
          (setf (type element) type)))
      (when (<= 3 (length form))
        (let ((mandatory (third form)))
          (assert (member mandatory '(:mandatory :optional))
                  () "~S must be one of :MANDATORY :OPTIONAL." mandatory)
          (setf (mandatoryp element) (eq mandatory :mandatory))))
      (when (<= 4 (length form))
        (let ((initial-value (fourth form)))
          (setf (initial-value element) initial-value)))
      element)))

(defmethod embed-documentation ((element protocol-config) (string string))
  (setf (documentation (name element) 'config) string))

(defmethod generate-forms ((element protocol-config))
  (let* ((name (name element)) (type (type element))
         (mandatoryp (mandatoryp element))
         (documentation (documentation name 'config)))
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
  (if (slot-boundp element '%initial-value)
      `(funcall *configuration-setter*
                ,(name element) ,(initial-value element))
      '()))

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
