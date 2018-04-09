;;;; protocol/protocol.lisp

(in-package #:protest/protocol)

(defvar *protocols* (make-hash-table)
  "A hash-table mapping from protocol names to protocol objects.")

(defvar *compile-time-protocols* (make-hash-table)
  "A hash-table mapping from protocol names to protocol objects, used for
compile-time constraint checking.")

(defvar *declaim-types* t
  "States if protocols should declaim function and variable types.")

(defclass protocol ()
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%whole :accessor whole
           :initarg :whole)
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

(defmethod make-load-form ((object protocol) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots object))

(defmethod initialize-instance :after
    ((protocol protocol) &key name dependencies export (declaim-type-p t))
  (when (or (null name) (not (symbolp name)))
    (protocol-error "NAME must be a non-null symbol, not ~S." name))
  (setf (name protocol) name)
  (setf (dependencies protocol) dependencies)
  (setf (exports protocol)
        (if (eq export 't) (compute-exports protocol) export))
  (let ((element-forms (cdddr (whole protocol))))
    (setf (elements protocol)
          (generate-elements element-forms declaim-type-p))))

(defmethod generate-code ((protocol protocol))
  (mappend #'generate-code (elements protocol)))

(defun generate-elements (elements declaim-type-p)
  (loop for sublist on elements
        for form = (first sublist)
        for string = (second sublist)
        for element = nil
        if (listp form)
          do (setf element
                   (generate-element (car form) (cdr form) declaim-type-p))
          and collect element
        if (and (listp form) (stringp string))
          do (embed-documentation element string)))

(defun compute-exports (protocol)
  (loop for element in (elements protocol)
        for name = (name element)
        when (symbolp name) collect name))

(defun validate-protocol (protocol hash-table)
  (check-dependencies-valid protocol hash-table)
  (check-duplicate-element-forms protocol)
  (check-duplicate-effective-elements protocol hash-table)
  (check-exports protocol))

(defun check-dependencies-valid (protocol hash-table)
  (let ((name (name protocol))
        (dependencies (dependencies protocol)))
    (unless (every (conjoin #'identity #'symbolp) dependencies)
      (protocol-error "Dependency is not a non-null symbol: ~A"
                      (find-if-not #'symbolp dependencies)))
    (when (member name dependencies)
      (protocol-error "Protocol ~A must not depend on itself." name))
    (loop for dependency in dependencies
          unless (gethash dependency hash-table)
            do (protocol-error "Unknown protocol ~A passed as a dependency."
                               dependency))
    (unless (setp dependencies)
      (protocol-error "Duplicate protocol dependencies detected: ~A"
                      (retain-duplicates dependencies)))
    (traverse-dependencies name dependencies hash-table)))

(defun traverse-dependencies (name dependencies hash-table)
  (loop with stack = dependencies
        with visited = (make-hash-table)
        for dependency = (pop stack)
        while dependency
        if (eq dependency name)
          do (protocol-error "Circular dependency detected for protocol ~A."
                             name)
        if (not (gethash dependency visited))
          do (setf (gethash dependency visited) t)
             (let* ((new-protocol (gethash dependency hash-table))
                    (new-names (dependencies new-protocol)))
               (dolist (new-name new-names) (push new-name stack)))
        finally (return visited)))

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

(defun check-duplicate-effective-elements (protocol hash-table)
  (let* ((name (name protocol))
         (dependencies (dependencies protocol))
         (symbols (traverse-dependencies name dependencies hash-table))
         (protocols (mapcar (rcurry #'gethash hash-table)
                            (hash-table-keys symbols)))
         (protocols (cons protocol protocols))
         (hash-table (make-hash-table :test #'equal)))
    (dolist (protocol protocols)
      (dolist (form (mapcar (compose #'first #'generate-forms)
                            (elements protocol)))
        (let* ((type (first form)) (name (second form)) (list (list type name)))
          (when (member type '(:category :config))
            (setf (first list) :category-or-config))
          (if (gethash list hash-table)
              (protocol-error "Duplicate element form for ~{~S ~S~} found ~
in protocol ~A." list (name protocol))
              (setf (gethash list hash-table) t)))))))

(defun check-exports (protocol)
  (let ((exports (exports protocol)))
    (assert (or (eq exports t) (and (listp exports) (every #'symbolp exports)))
            () "Incorrect export list: ~S" exports)
    (loop for export in exports
          for element = (find export (elements protocol) :key #'name)
          unless element do (error "Export not found: ~S" export))))

(defmacro define-protocol (&whole whole name (&rest options) &body forms)
  "Defines the protocol with the provided NAME and OPTIONS, instantiating all
its elements based on FORMS."
  (declare (ignore forms))
  (let* ((protocol (apply #'make-instance 'protocol
                          :name name :whole whole options)))
    (validate-protocol protocol *compile-time-protocols*)
    (setf (gethash name *compile-time-protocols*) protocol)
    (with-gensyms (value foundp)
      `(progn
         (validate-protocol ,protocol *protocols*)
         (multiple-value-bind (,value ,foundp) (gethash ',name *protocols*)
           (when (and ,foundp (not (equalp (whole ,value) (whole ,protocol))))
             (warn "Redefining ~A in DEFINE-PROTOCOL" ',name)))
         ,@(generate-code protocol)
         (setf (gethash ',name *protocols*) ,protocol)))))
