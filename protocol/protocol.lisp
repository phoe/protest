;;;; protocol/protocol.lisp

(in-package #:protest/protocol)

(defvar *protocols* (make-hash-table)
  "A hash-table mapping from protocol names to protocol objects.")

(defvar *declaim-types* t
  "States if protocols should declaim function and variable types.")

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
  (setf (dependencies protocol) dependencies))

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
                      (new-names (dependencies new-protocol)))
                 (dolist (new-name new-names) (push new-name stack))))))

(defmacro define-protocol (&whole whole name (&rest options) &body forms)
  (with-gensyms (protocol elements)
    `(let ((,protocol (apply #'make-instance 'protocol
                             :name ',name :form ',whole ',options)))
       (multiple-value-bind (value foundp) (gethash ',name *protocols*)
         (when (and foundp (not (equalp (form value) (form ,protocol))))
           (warn "Redefining ~A in DEFINE-PROTOCOL" ',name)))
       (setf (gethash ',name *protocols*) ,protocol)
       ,(check-duplicate-element-forms forms)
       (let ((,elements (generate-elements ,protocol ',forms)))
         (setf (elements ,protocol) ,elements))
       ',name)))

(defun check-duplicate-element-forms (forms)
  (loop with hash-table = (make-hash-table :test #'equal)
        for form in forms
        for type = (first form)
        for name = (second form)
        for list = (list type name)
        when (member type '(:category :config))
          do (setf (first list) :category-or-config)
        if (gethash list hash-table)
          do (protocol-error "Duplicate element form for ~{~S ~S~}." list)
        else do (setf (gethash list hash-table) t)))

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

(defmacro defgeneric? (name lambda-list &body options)
  (if (or (not (fboundp name))
          (not (typep (fdefinition name) 'generic-function)))
      `(defgeneric ,name ,lambda-list ,@options)
      `(progn)))
