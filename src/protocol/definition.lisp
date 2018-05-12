;;;; src/protocol/definition.lisp

(in-package #:protest/protocol)

(defun retain-duplicates (list)
  (loop with r = '() for i in list
        if (find i r) collect i else do (push i r)))

(defvar *protocols* (make-hash-table)
  "A hash-table mapping from protocol names to protocol objects.")

(defun find-protocol (name)
  "Returns the protocol with the provided name."
  (values (gethash name *protocols*)))

(defun (setf find-protocol) (new-value name)
  "Sets the protocol with the provided name."
  (check-type new-value (or protocol null))
  (if new-value
      (setf (gethash name *protocols*) new-value)
      (remhash name *protocols*))
  new-value)

(defclass protocol ()
  ((%name :reader name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%whole :accessor whole
           :initarg :whole)
   (%tags :accessor tags
          :initarg :tags
          :initform '())
   (%attachments :accessor attachments
                 :initarg :attachments
                 :initform '())
   (%dependencies :reader dependencies
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

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t)
    (princ (name protocol) stream)))

(defmethod make-load-form ((protocol protocol) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots protocol))

(defmethod initialize-instance :after
    ((protocol protocol)
     &key name dependencies export (declaim-types-p t) documentation)
  (when (or (null name) (not (symbolp name)))
    (protocol-error "NAME must be a non-null symbol, not ~S." name))
  (when documentation
    (unless (typep documentation 'string)
      (protocol-error "DOCUMENTATION must be a string, not ~A."
                      documentation))
    (setf (documentation protocol 'protocol) documentation))
  (setf (slot-value protocol '%name) name
        (slot-value protocol '%dependencies) dependencies
        (exports protocol)
        (if (eq export 't) (compute-exports protocol) export))
  (let ((element-forms (cdddr (whole protocol))))
    (setf (elements protocol)
          (generate-elements element-forms declaim-types-p))))

(defvar *protocol-documentation-store* (make-hash-table))

(defmethod documentation ((slotd symbol) (type (eql 'protocol)))
  (gethash slotd *protocol-documentation-store*))

(defmethod documentation ((slotd protocol) (type (eql 'protocol)))
  (gethash (name slotd) *protocol-documentation-store*))

(defmethod (setf documentation)
    (new-value (slotd symbol) (type (eql 'protocol)))
  (setf (gethash slotd *protocol-documentation-store*) new-value))

(defmethod (setf documentation)
    (new-value (slotd protocol) (type (eql 'protocol)))
  (setf (gethash (name slotd) *protocol-documentation-store*) new-value))

(defmethod generate-code ((protocol protocol))
  `(progn ,@(mappend #'generate-code (elements protocol))
          (export ',(exports protocol))
          (values)))

(defun generate-elements (elements declaim-types-p)
  (loop for sublist on elements
        for form = (first sublist)
        for string = (second sublist)
        for element = nil
        if (listp form)
          do (setf element
                   (generate-element (car form) (cdr form) declaim-types-p))
          and collect element
        if (and (listp form) (stringp string))
          do (setf (docstring element) string)))

(defun compute-exports (protocol)
  (loop for element in (elements protocol)
        for name = (name element)
        when (symbolp name) collect name))

(defun protocol-effective-dependencies (protocol)
  (loop with name = (name protocol)
        with dependencies = (dependencies protocol)
        with visited = (make-hash-table)
        with stack = dependencies
        for dependency = (pop stack)
        while dependency
        if (eq dependency name)
          do (protocol-error "Circular dependency detected for protocol ~A."
                             name)
        if (not (gethash dependency visited))
          do (setf (gethash dependency visited) t)
             (let* ((new-protocol (find-protocol dependency))
                    (new-names (dependencies new-protocol)))
               (dolist (new-name new-names) (push new-name stack)))
        finally (return (hash-table-keys visited))))

(defun compute-effective-protocol-elements (protocol)
  "Returns a fresh list of all protocol elements that occur inside the provided
protocol and all of its dependencies, including transitive ones."
  (check-type protocol protocol)
  (let* ((symbols (cons (name protocol)
                        (protocol-effective-dependencies protocol)))
         (protocols (mapcar #'find-protocol symbols)))
    (mappend #'elements protocols)))
