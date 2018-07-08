;;;; src/protocol/elements/category.lisp

(in-package #:protest/protocol)

(defvar *category-callback* (constantly nil)
  "A function of one argument used as a callback for declaring categories. The
only argument is the category name.")

(defclass protocol-category (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME.")))
  (:documentation
   "Describes a protocol configuration category that is a part of a protocol.
\
The form for a protocol configuration category consits of the following
subforms:
* NAME - mandatory, must be a list of keywords and symbols. Denotes the name of
  the configuration category. The canonical names (with non-keyword symbols
  replaced by NIL) of configuration entries and configuration categories must
  not collide with each other."))

(defmethod keyword-element-class ((keyword (eql :category)))
  (find-class 'protocol-category))

(defmethod generate-element-using-class
    ((class (eql (find-class 'protocol-category)))
     details &optional declaim-type-p)
  (declare (ignore declaim-type-p))
  (destructuring-bind (name) details
    (assert (and (consp name) (every #'symbolp name)) ()
            "Wrong thing to be a configuration category name: ~A" name)
    (let ((element (make-instance class :name name)))
      element)))

(defmethod generate-forms ((element protocol-category))
  (let* ((name (name element))
         (documentation (docstring element)))
    `((:category ,name)
      ,@(when documentation `(,documentation)))))

(defmethod generate-code ((element protocol-category))
  `((funcall *category-callback* ',(name element))
    ,@(when-let ((documentation (docstring element)))
        `((setf (documentation ',(name element) 'category) ,documentation)))))

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

(defmethod canonical-name ((category protocol-category))
  (canonicalize-name (name category)))
