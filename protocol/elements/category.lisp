;;;; protocol/elements/category.lisp

(in-package #:protest/protocol)

(defclass protocol-category (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME.")))
  (:documentation
   "Describes a protocol configuration category that is a part of a protocol.
\
The form for a protocol configuration category consits of the following
subforms:
* NAME - mandatory, must be a list of keywords. Denotes the name of the
  configuration category. The name of configuration entries and configuration
  categories must not collide with each other."))

(defmethod generate-element
    ((type (eql :category)) form &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name) form
    (assert (every #'keywordp name)
            () "Wrong thing to be a configuration category name: ~A" name)
    (let ((element (make-instance 'protocol-category :name name)))
      element)))

(defmethod generate-forms ((element protocol-category))
  (let* ((name (name element))
         (documentation (docstring element)))
    `((:category ,name)
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-category))
  (when-let ((documentation (docstring element)))
    `((setf (documentation ',(name element) 'category) ,documentation))))

(defvar *category-documentation-store*
  (make-hash-table :test #'equal))

(defmethod documentation ((slotd list) (doc-type (eql 'category)))
  (values (gethash slotd *category-documentation-store*)))

(defmethod (setf documentation)
    (new-value (slotd list) (doc-type (eql 'category)))
  (if new-value
      (setf (gethash slotd *category-documentation-store*) new-value)
      (remhash slotd *category-documentation-store*))
  new-value)
