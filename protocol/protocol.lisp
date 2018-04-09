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
  (when (or (null name) (not (symbolp name)))
    (protocol-error "NAME must be a non-null symbol, not ~S." name))
  (setf (name protocol) name)
  (validate-dependencies protocol dependencies)
  (setf (dependencies protocol) dependencies
        (exports protocol) export))

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
  (traverse-dependencies protocol dependencies))

(defun traverse-dependencies (protocol dependencies)
  (loop with stack = dependencies
        with visited = (make-hash-table)
        with name = (name protocol)
        for dependency = (pop stack)
        while dependency
        if (eq dependency name)
          do (protocol-error "Circular dependency detected for protocol ~A."
                             name)
        if (not (gethash dependency visited))
          do (setf (gethash dependency visited) t)
             (let* ((new-protocol (gethash dependency *protocols*))
                    (new-names (dependencies new-protocol)))
               (dolist (new-name new-names) (push new-name stack)))
        finally (return visited)))

(defmacro define-protocol (&whole whole name (&rest options) &body forms)
  (with-gensyms (value foundp)
    (let ((protocol (instantiate-protocol whole name options forms)))
      `(progn
         (multiple-value-bind (,value ,foundp) (gethash ',name *protocols*)
           (when (and ,foundp (not (equalp (form ,value) (form ,protocol))))
             (warn "Redefining ~A in DEFINE-PROTOCOL" ',name)))
         (setf (gethash ',name *protocols*) ,protocol)
         ,@(mappend #'generate-code (elements protocol))
         ',name))))

(defmethod make-load-form ((object protocol) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots object))

(defun instantiate-protocol (whole name options forms)
  (let ((protocol (apply #'make-instance 'protocol
                         :name name :form whole options)))
    (check-duplicate-element-forms forms)
    (setf (elements protocol) (generate-elements forms))
    (check-duplicate-effective-elements protocol)
    (compute-and-validate-exports protocol)
    protocol))

(defun retain-duplicates (list)
  (loop with r = '() for i in list
        if (find i r) collect i else do (push i r)))

(defun generate-elements (elements)
  (loop for sublist on elements
        for element-form = (first sublist)
        for string = (second sublist)
        for element = nil
        if (listp element-form)
          do (setf element (apply #'generate-element element-form))
          and collect element
        if (and (listp element-form) (stringp string))
          do (embed-documentation element string)))

(defun check-duplicate-element-forms (forms)
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
        else do (setf (gethash list hash-table) t)))

(defun check-duplicate-effective-elements (protocol)
  (let* ((symbols (traverse-dependencies protocol (dependencies protocol)))
         (protocols (mapcar (rcurry #'gethash *protocols*)
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

(defun compute-and-validate-exports (protocol)
  (let ((exports (exports protocol)))
    (assert (or (eq exports t) (and (listp exports) (every #'symbolp exports)))
            () "Incorrect export list: ~S" exports)
    (if (eq exports t)
        (loop for element in (elements protocol)
              for name = (name element)
              when (symbolp name)
                collect name into names
              finally (setf (exports protocol) names))
        (loop for export in exports
              for element = (find export (elements protocol) :key #'name)
              unless element do (error "Export not found: ~S" export)))))
