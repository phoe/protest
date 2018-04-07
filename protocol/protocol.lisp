;;;; protocol/protocol.lisp

(in-package #:protest/protocol)

(defvar *protocols* (make-hash-table))

(defclass protocol ()
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%form :accessor form
          :initarg :form)
   (%description :accessor description
                 :initarg :description
                 :initform nil)
   (%tags :accessor tags
          :initarg :tags
          :initform '())
   (%dependencies :accessor dependencies
                  :initform '())
   (%exports :accessor exports
             :initarg :exports
             :initform '())
   (%elements :accessor elements
              :initarg :elements
              :initform '()))
  (:documentation
   "Describes a protocol understood as a relation between data types and
operations on these types."))

(defmethod print-object ((object protocol) stream)
  (print-unreadable-object (object stream :type t)
    (princ (name object) stream)))

(defmethod initialize-instance :after
    ((protocol protocol) &key name dependencies export)
  (declare (ignore export)) ;; TODO
  (when (not (symbolp name))
    (protocol-error "NAME must be a symbol."))
  (setf (name protocol) name)
  (validate-dependencies protocol dependencies)
  (setf (dependencies protocol)
        (mapcar (rcurry #'gethash *protocols*) dependencies)))

(defun validate-dependencies (protocol dependencies)
  (unless (every #'symbolp dependencies)
    (protocol-error "Dependency is not a symbol: ~A"
                    (find-if #'symbolp dependencies)))
  (loop for dependency in dependencies
        unless (gethash dependency *protocols*)
          do (protocol-error "Unknown protocol ~A passed as a dependency."
                             dependency))
  (when (member (name protocol) dependencies)
    (protocol-error "Protocol ~A must not depend on itself." (name protocol)))
  (unless (setp dependencies)
    (protocol-error "Duplicate protocol dependencies detected: ~A"
                    (retain-duplicates dependencies)))
  (let ((stack dependencies)
        (visited (make-hash-table)))
    (loop with name = (name protocol)
          for dependency = (pop stack)
          while dependency
          if (eq dependency name)
            do (protocol-error "Circular dependency detected for protocol ~A."
                               name)
          if (not (gethash dependency visited))
            do (setf (gethash dependency visited) t)
               (let* ((new-protocol (gethash dependency *protocols*))
                      (new-dependencies (dependencies new-protocol))
                      (new-names (mapcar #'name new-dependencies)))
                 (dolist (new-name new-names) (push new-name stack))))))

(defmacro define-protocol (&whole whole name (&rest options) &body elements)
  (declare (ignore elements))
  `(let ((protocol (apply #'make-instance 'protocol
                          :name ',name :form ',whole ',options)))
     (multiple-value-bind (value foundp) (gethash ',name *protocols*)
       (when (and foundp (not (equalp (form value) (form protocol))))
         (warn "Redefining ~A in DEFINE-PROTOCOL" ',name)))
     (setf (gethash ',name *protocols*) protocol)
     (generate-elements protocol elements)
     ',name))

(defun retain-duplicates (list)
  (loop with r = '() for i in list
        if (find i r) collect i else do (push i r)))

(defun generate-elements (protocol elements)
  (loop for sublist on elements
        for element-form = (first sublist)
        for string = (second sublist)
        for element = nil
        if (listp element-form)
          do (setf element (apply #'generate-element element-form))
             (push element (elements protocol))
        if (and (listp element-form) (stringp string))
          do (embed-documentation element string)))




















(defclass protocol-macro (protocol-operation)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME.")))
  (:documentation
   "Describes a protocol macro that is a part of a protocol."))

(defmethod generate-element ((type (eql :macro)) &rest form)
  (error "not implemented"))

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
   "Describes a protocol class that is a part of a protocol."))

(defmethod generate-element ((type (eql :class)) &rest form)
  (error "not implemented"))

(defclass protocol-condition-type (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%supertypes :accessor supertypes
                :initarg :supertypes
                :initform '())
   (%slots :accessor slots
           :initarg :slots
           :initform '())
   (%options :accessor options
             :initarg :options
             :initform '()))
  (:documentation
   "Describes a protocol condition type that is a part of a protocol."))

(defmethod generate-element ((type (eql :condition-type)) &rest form)
  (error "not implemented"))

(defclass protocol-variable (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%type :accessor type
          :initarg :type
          :initform t)
   (%default-value :accessor default-value
                   :initarg :default-value))
  (:documentation
   "Describes a protocol variable that is a part of a protocol."))

(defmethod generate-element ((type (eql :variable)) &rest form)
  (error "not implemented"))

(defclass protocol-category (protocol-data-type)
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME.")))
  (:documentation
   "Describes a protocol configuration category that is a part of a protocol."))

(defmethod generate-element ((type (eql :category)) &rest form)
  (error "not implemented"))

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
   (%default-value :accessor default-value
                   :initarg :default-value))
  (:documentation
   "Describes a protocol configuration entry that is a part of a protocol."))

(defmethod generate-element ((type (eql :config)) &rest form)
  (error "not implemented"))
