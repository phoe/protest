;;;; src/test-case/test-case.lisp

(in-package #:protest/test-case)

(defvar *test-cases* (make-hash-table :test #'equal)
  "A mapping from test case names to test cases.")

(defun find-test-case (name &optional (package *package*))
  "Returns the test case with the provided name from the provided package."
  (values (gethash (cons (string name) (find-package package)) *test-cases*)))

(defun (setf find-test-case) (new-value name &optional (package *package*))
  "Sets the test case with the provided name with the provided package."
  (check-type new-value (or test-case null))
  (if new-value
      (setf (gethash (cons (string name) (find-package package)) *test-cases*)
            new-value)
      (remhash (cons (string name) (find-package package)) *test-cases*)))

(defclass test-case ()
  ((%name :reader name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%package :reader package-of
             :initarg :package
             :initform *package*)
   (%whole :accessor whole
           :initarg :whole)
   (%tags :accessor tags
          :initarg :tags
          :initform '())
   (%attachments :accessor attachments
                 :initarg :attachments
                 :initform '())
   (%steps :accessor steps
           :initarg :steps
           :initform (make-hash-table)))
  (:documentation
   "Describes a test case understood as a series of test phases and test steps
describing each part of the test."))

(defmethod steps-list ((test-case test-case))
  (sort (hash-table-values (steps test-case)) #'< :key #'id))

(defmethod print-object ((test-case test-case) stream)
  (print-unreadable-object (test-case stream :type t)
    (princ (name test-case) stream)))

(defmethod make-load-form ((test-case test-case) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots test-case))

(defmethod initialize-instance :after
    ((test-case test-case) &key name documentation)
  (unless (and name (typep name 'string-designator))
    (protocol-error "Wrong thing to be a test case name: ~A" name))
  (setf (slot-value test-case '%name) (string name))
  (when documentation
    (unless (typep documentation 'string)
      (protocol-error "DOCUMENTATION must be a string, not ~A."
                      documentation))
    (setf (documentation test-case'test-case) documentation))
  (let ((step-forms (cdddr (whole test-case))))
    (setf (steps test-case)
          (generate-steps step-forms))))

(defvar *test-case-documentation-store* (make-hash-table))

(defmethod documentation ((slotd string) (type (eql 'test-case)))
  (gethash slotd *test-case-documentation-store*))

(defmethod documentation ((slotd symbol) (type (eql 'test-case)))
  (gethash (string slotd) *test-case-documentation-store*))

(defmethod documentation ((slotd test-case) (type (eql 'test-case)))
  (gethash (name slotd) *test-case-documentation-store*))

(defmethod (setf documentation)
    (new-value (slotd string) (type (eql 'test-case)))
  (setf (gethash slotd *test-case-documentation-store*) new-value))

(defmethod (setf documentation)
    (new-value (slotd symbol) (type (eql 'test-case)))
  (setf (gethash (string slotd) *test-case-documentation-store*) new-value))

(defmethod (setf documentation)
    (new-value (slotd test-case) (type (eql 'test-case)))
  (setf (gethash (name slotd) *test-case-documentation-store*) new-value))

(defun generate-steps (forms)
  (let ((forms forms) (current-phase nil) (result (make-hash-table)))
    (flet ((make (id description)
             (make-instance 'test-step :id id :description description
                                       :test-phase current-phase)))
      (do () ((null forms))
        (let ((elt (pop forms)))
          (typecase elt
            (unsigned-byte
             (let ((string (pop forms)))
               (unless (typep string 'string)
                 (protocol-error "Wrong thing in a test case definition: ~S"
                                 string))
               (setf (gethash elt result) (make elt string))))
            (symbol (setf current-phase elt))
            (t (protocol-error "Wrong thing in a test case definition: ~S"
                               elt)))))
      result)))

(defun validate-test-case (test-case)
  (check-duplicate-ids test-case)
  (check-duplicate-test-phases test-case)
  (check-id-order test-case))

(defun check-duplicate-ids (test-case)
  (loop with hash-table = (make-hash-table)
        for test-step in (steps-list test-case)
        for id = (id test-step)
        for (value foundp) = (multiple-value-list (gethash id hash-table))
        if foundp
          do (protocol-error "Duplicate step ID ~D found." id)
        else do (setf (gethash id hash-table) t)))

(defun check-duplicate-test-phases (test-case)
  (loop with hash-table = (make-hash-table)
        with forms = (cdddr (whole test-case))
        for symbol in (remove-if-not #'symbolp forms)
        for (value foundp) = (multiple-value-list (gethash symbol hash-table))
        if foundp
          do (protocol-error "Duplicate test phase ~A found." symbol)
        else do (setf (gethash symbol hash-table) t)))

(defun check-id-order (test-case)
  (loop with forms = (cdddr (whole test-case))
        with previous-number = -1
        for number in (remove-if-not #'integerp forms)
        if (< previous-number number)
          do (setf previous-number number)
        else do (protocol-error "Test step IDs not in order: ~D came after ~D."
                                previous-number number)))

(defun ensure-test-case (name options whole &optional (package *package*))
  (unless (and name (typep name 'string-designator))
    (protocol-error "Wrong thing to be a test case name: ~A" name))
  (unless (stringp name) (setf name (string name)))
  (let ((test-case (apply #'make-instance 'test-case
                          :name name :whole whole options)))
    (validate-test-case test-case)
    (let ((value (find-test-case name package)))
      (when (and value (not (equalp (whole value) (whole test-case))))
        (warn "Redefining ~A in DEFINE-PROTOCOL" name)))
    (setf (find-test-case name package) test-case)
    name))

(defmacro define-test-case (&whole whole name (&rest options) &body steps)
  (declare (ignore steps))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-test-case ',name ',options ',whole)))
