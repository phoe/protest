;;;; src/protocol/protocol.lisp

(in-package #:protest/protocol)

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

(defun validate-protocol (protocol)
  (check-dependencies-valid protocol)
  (check-duplicate-element-forms protocol)
  (check-duplicate-effective-elements protocol)
  (check-exports protocol))
;; TODO after redefining a protocol it is possible that it now has collisions
;; with protocols that depend on it. Make sure that this does not happen and
;; write a test for it.

(defun check-dependencies-valid (protocol)
  (let ((name (name protocol))
        (dependencies (dependencies protocol)))
    (unless (every (conjoin #'identity #'symbolp) dependencies)
      (protocol-error "Dependency is not a non-null symbol: ~A"
                      (find-if-not #'symbolp dependencies)))
    (when (member name dependencies)
      (protocol-error "Protocol ~A must not depend on itself." name))
    (loop for dependency in dependencies
          unless (find-protocol dependency)
            do (protocol-error "Unknown protocol ~A passed as a dependency."
                               dependency))
    (unless (setp dependencies)
      (protocol-error "Duplicate protocol dependencies detected: ~A"
                      (retain-duplicates dependencies)))
    (traverse-dependencies protocol)))

(defun traverse-dependencies (protocol)
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

(defun retain-duplicates (list)
  (loop with r = '() for i in list
        if (find i r) collect i else do (push i r)))

(defun check-duplicate-element-forms (protocol)
  (let ((forms (cdddr (whole protocol))))
    (setf forms (remove-if #'stringp forms))
    (loop with hash-table = (make-hash-table :test #'equal)
          for form in forms
          for type = (first form)
          for name = (second form)
          for list = (list type name)
          when (member type '(:category :config))
            do (setf (first list) :category-or-config)
          if (gethash list hash-table)
            do (protocol-error "Duplicate element form for ~{~S ~S~}." list)
          else do (setf (gethash list hash-table) t))))

(defun check-duplicate-effective-elements (protocol)
  (let* ((symbols (traverse-dependencies protocol))
         (protocols (mapcar #'find-protocol symbols))
         (protocols (cons protocol protocols))
         (hash-table (make-hash-table :test #'equal)))
    (dolist (protocol protocols)
      (dolist (form (mapcar (compose #'first #'generate-forms)
                            (elements protocol)))
        (let* ((type (first form)) (name (second form)) (list (list type name)))
          (setf (first list)
                (case type
                  ((:class :condition-type) :class-or-condiiton-type)
                  ((:function :macro) :function-or-macro)
                  ((:category :config) :category-or-config)
                  (t (first list))))
          (if (gethash list hash-table)
              (protocol-error "Duplicate element form for ~{~S ~S~} found ~
in protocol ~A." list (name protocol))
              (setf (gethash list hash-table) t)))))))

(defun check-exports (protocol)
  (let ((exports (exports protocol)))
    (assert (or (eq exports t) (and (listp exports) (every #'symbolp exports)))
            () "Incorrect export list: ~S" exports)
    (unless (eq exports t)
      (loop for export in exports
            for element = (find export (elements protocol) :key #'name)
            unless element do (error "Export not found: ~S" export)))))

(defun ensure-protocol (name options whole)
  (let* ((protocol (apply #'make-instance 'protocol
                          :name name :whole whole options)))
    (validate-protocol protocol)
    (let ((value (find-protocol name)))
      (when (and (find-protocol name)
                 (not (equalp (whole value) (whole protocol))))
        (warn "Redefining ~A in DEFINE-PROTOCOL" name)))
    (setf (find-protocol name) protocol)
    name))

(defmacro define-protocol (&whole whole name (&rest options) &body forms)
  "Defines the protocol with the provided NAME and OPTIONS, instantiating all
its elements based on FORMS."
  (declare (ignore forms))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-protocol ',name ',options ',whole)))

(defmacro execute-protocol (name)
  "Executes all the side effects of the protocol with the provided NAME."
  (check-type name symbol)
  (let ((protocol (find-protocol name)))
    (if protocol
        (progn (validate-protocol protocol)
               (generate-code protocol))
        (error "Protocol ~S was not found." name))))

(defun compute-effective-protocol-elements (protocol)
  "Returns a fresh list of all protocol elements that occur inside the provided
protocol and all of its dependencies, including transitive ones."
  (check-type protocol protocol)
  (let* ((symbols (cons (name protocol) (traverse-dependencies protocol)))
         (protocols (mapcar #'find-protocol symbols)))
    (mappend #'elements protocols)))

;; TODO check that all exported symbols are documented
