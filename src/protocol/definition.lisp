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
  (cond (new-value
         (setf (gethash name *protocols*) new-value))
        (t
         (remhash name *protocols*)
         (setf (documentation name 'protocol) nil)))
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
   (%bindings :accessor bindings
              :initarg :bindings
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
    (setf (documentation protocol 'protocol) (format nil documentation)))
  (setf (slot-value protocol '%name) name
        (slot-value protocol '%dependencies) dependencies)
  (let ((element-forms (cdddr (whole protocol))))
    (setf (elements protocol)
          (generate-elements element-forms declaim-types-p)
          (exports protocol)
          (if (eq export 't) (compute-exports protocol) export))))

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
  (let ((contents `((progn)
                    ,@(mappend #'generate-code (elements protocol))
                    (export ',(exports protocol))
                    (values)))
        (bindings (bindings protocol)))
    (if (null bindings)
        `(progn ,@contents)
        `(let ,bindings ,@contents))))

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
          do (setf (docstring element) (format nil string))))

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

(defgeneric validate-implementations (protocol &key errorp successp)
  (:documentation "Checks if all subclasses of protocol classes defined
in the protocol have appropriate methods defined on protocol functions defined
in the protocol. Returns a list of validation errors if the check fails or if
any protocol function is undefined. Each entry in the list follows the pattern:
* (:missing-method FUNCTION POSITION PROTOCOL-CLASS CONCRETE-CLASS): There is no
  method defined on the protocol function that accepts the concrete class as its
  POSITIONth argument on the given position, but that class is a subtype of
  protocol class that is the declared protocol specializer of that argument.
  If ERRORP is true, a condition of type PROTOCOL-VALIDATION-ERROR is instead
  signaled.
* (:unbound-function FUNCTION-NAME): The protocol function with this name is
  undefined. The protocol might not have been executed. If ERRORP is true, a
  condition of type UNDEFINED-PROTOCOL-FUNCTION is instead signaled.
* (:success FUNCTION POSITION PROTOCOL-CLASS CONCRETE-CLASS SPECIALIZER):
  Denotes that the function has a method specialized on the specializer on
  POSITIONth position and that specializer is a proper subtype of the concrete
  class. (In other words, there is a method defined on that class.)
  This entry is only collected if the SUCCESSP argument is true.")
  ;; TODO Better multimethod validation. Currently the arguments are validated
  ;; one by one, so if you define a method on protocol classes C1 and C2 with
  ;; their respective subclasses S1 and S2 and you create methods specializing
  ;; on (S1 X) and (X S2), there is still no meaningful way to call the generic
  ;; function on elements of type S1 and S2, which means that the protocol is
  ;; not followed. The current algorithm does not catch these situations and
  ;; therefore needs to be rethought.
  (:method ((protocol symbol) &rest rest)
    (apply #'validate-implementations (find-protocol protocol) rest))
  (:method ((protocol protocol) &key errorp successp)
    (uiop:while-collecting (collect)
      (dolist (protocol-function (remove 'protocol-function (elements protocol)
                                         :test-not #'eql :key #'type-of))
        (let ((function-name (name protocol-function)))
          (cond
            ((and (not (fboundp function-name)) errorp)
             (error 'undefined-protocol-function :name function-name))
            ((and (not (fboundp function-name)) (not errorp))
             (collect (list :unbound-function function-name)))
            (t
             (let* ((function (fdefinition function-name))
                    (methods (generic-function-methods function))
                    (specializers (mapcar #'method-specializers methods))
                    (lambda-list (standard-specializers-only
                                  (lambda-list protocol-function)))
                    (class-names (extract-specializer-names lambda-list)))
               (dotimes (position (length class-names))
                 (let* ((class-name (nth position class-names))
                        (class (find-class class-name)))
                   (when (protocol-object-p class)
                     (dolist (subclass (concrete-subclasses class))
                       (let* ((candidates (mapcar (curry #'nth position)
                                                  specializers))
                              (specializer (find subclass candidates
                                                 :test #'subtypep)))
                         (cond (specializer
                                (when successp
                                  (collect (list :success function position
                                                 class subclass specializer))))
                               (errorp
                                (error 'protocol-validation-error
                                       :function function :subclass subclass
                                       :position position :class class))
                               (t (collect (list :missing-method
                                                 function position
                                                 class subclass)))))))))))))))))

(defun concrete-subclasses (class)
  (remove-if #'protocol-object-p (moptilities:subclasses class)))

(defun standard-specializers-only (lambda-list)
  (loop for elt in lambda-list
        when (and (second elt) (symbolp (second elt)))
          collect elt))

(defgeneric remove-protocol (protocol)
  (:documentation "Removes the provided protocol and all the effects of its
elements from the Lisp image.")
  (:method ((protocol symbol))
    (remove-protocol (find-protocol protocol)))
  (:method ((protocol protocol))
    (mapc #'remove-protocol-element (elements protocol))
    (setf (find-protocol (name protocol)) nil)))

(defvar *undefined-protocol-function-report*
  "The protocol function ~S is undefined. (Has the protocol been executed?)")

(defun undefined-protocol-function-report (condition stream)
  (format stream *undefined-protocol-function-report*
          (cell-error-name condition)))

(define-condition undefined-protocol-function
    (undefined-function protocol-error) ()
  (:report undefined-protocol-function-report))

(defvar *protocol-validation-error-report*
  "There is no method defined on protocol function ~S that accepts concrete ~
class ~S as its argument on the ~:R position, but that class is a subtype of ~
protocol class ~S that is the declared protocol specializer of that argument.")

(defun protocol-validation-error-report (condition stream)
  (format stream *protocol-validation-error-report*
          (protocol-validation-error-function condition)
          (protocol-validation-error-subclass condition)
          (protocol-validation-error-position condition)
          (protocol-validation-error-class condition)))

(define-condition protocol-validation-error (protocol-error)
  ((function :reader protocol-validation-error-function
             :initarg :function
             :initform (required-argument :function))
   (subclass :reader protocol-validation-error-subclass
             :initarg :subclass
             :initform (required-argument :subclass))
   (class :reader protocol-validation-error-class
          :initarg :class
          :initform (required-argument :class))
   (position :reader protocol-validation-error-position
             :initarg :position
             :initform (required-argument :position)))
  (:report protocol-validation-error-report))
