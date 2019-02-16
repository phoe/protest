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
     &key name dependencies export declaim-types-p documentation)
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

(defgeneric remove-protocol (protocol)
  (:documentation "Removes the provided protocol and all the effects of its
elements from the Lisp image.")
  (:method ((protocol symbol))
    (remove-protocol (find-protocol protocol)))
  (:method ((protocol protocol))
    (mapc #'remove-protocol-element (elements protocol))
    (setf (find-protocol (name protocol)) nil)))

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
* (:undefined-function FUNCTION-NAME): The protocol function with this name is
  undefined. The protocol might not have been executed. If ERRORP is true, a
  condition of type UNDEFINED-PROTOCOL-FUNCTION is instead signaled.
* (:success FUNCTION POSITION PROTOCOL-CLASS CONCRETE-CLASS SPECIALIZER):
  Denotes that the function has a method specialized on the specializer on
  POSITIONth position and that specializer is a proper subtype of the concrete
  class. (In other words, there is a method defined on that class.)
  This entry is only collected if the SUCCESSP argument is true.")
  (:method ((protocol null) &rest rest)
    (declare (ignore rest))
    (error "NIL is not a valid protocol designator."))
  (:method ((protocol-name symbol) &rest rest)
    (let ((protocol (find-protocol protocol-name)))
      (if protocol
          (apply #'validate-implementations protocol rest)
          (error "Protocol ~S not found." protocol-name))))
  (:method ((protocol protocol) &key errorp successp)
    (uiop:while-collecting (collect)
      (dolist (protocol-function (remove 'protocol-function (elements protocol)
                                         :test-not #'eql :key #'type-of))
        (let ((function-name (name protocol-function)))
          (cond
            ((fboundp function-name)
             (mapcar #'collect
                     (validate-function protocol-function errorp successp)))
            (errorp (error 'undefined-protocol-function :name function-name))
            (t (collect (list :undefined-function function-name)))))))))

;;; VALIDATE-FUNCTION

(defun validate-function (protocol-function &optional errorp successp)
  (uiop:while-collecting (collect)
    (let ((function (fdefinition (name protocol-function)))
          (specializers (protocol-function-specializers protocol-function)))
      (dotimes (n (length specializers))
        (let ((specializer (nth n specializers)))
          (when (and (classp specializer) (protocol-object-p specializer))
            (dolist (subclass (concrete-subclasses specializer))
              (let* ((new-specializers (replace-one subclass n specializers))
                     (method (find-valid-method function new-specializers)))
                (assert (not (equal specializers new-specializers)))
                (cond (method
                       (when successp
                         (collect (list :success function n
                                        specializer subclass
                                        (nth n (method-specializers method))))))
                      ((not errorp)
                       (collect (list :missing-method
                                      function n specializer subclass)))
                      (t
                       (error 'protocol-validation-error
                              :function function :subclass subclass
                              :position n :class specializer)))))))))))

(defun find-valid-method (function specializers)
  (flet
      ((fn (function &rest specializers)
         (when-let ((method (find-method function '() specializers nil)))
           (return-from find-valid-method method)))
       (process (thing)
         (cond ((not (classp thing)) (list thing))
               ((protocol-object-p thing) (mopu:subclasses thing :proper? nil))
               (t (mopu:superclasses thing :proper? nil)))))
    (let ((product (mapcar #'process specializers)))
      (apply #'map-product (curry #'fn function) product)
      nil)))

(defun concrete-subclasses (class)
  (remove-if #'protocol-object-p (mopu:subclasses class :proper? nil)))

(defun protocol-superclasses (class)
  (remove-if-not #'protocol-object-p (mopu:subclasses class :proper? nil)))

(defun replace-one (element position sequence)
  (substitute-if element (constantly t) sequence
                 :start position :end (1+ position)))

(defun protocol-function-specializers (protocol-function)
  (flet ((find-specializer (thing)
           (etypecase thing
             ((cons (eql eql) (cons t null))
              (intern-eql-specializer (second thing)))
             (symbol (or (find-class thing nil) t)))))
    (let* ((lambda-list (lambda-list protocol-function))
           (specializer-names (extract-specializer-names lambda-list)))
      (mapcar #'find-specializer specializer-names))))

;;; CONDITIONS

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
