;;;; test-case/test-case.lisp

(in-package #:protest/test-case)

(defvar *test-cases* (make-hash-table :test #'equal)
  "A mapping from test case names to test cases.")

(defclass test-case ()
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
   (%attachments :accessor attachments
                 :initarg :attachments
                 :initform '()) ;; TODO attachments for protocols
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

(defmethod initialize-instance :after ((test-case test-case) &key name)
  (unless (and name (typep name 'string-designator))
    (protocol-error "Wrong thing to be a test case name: ~A" name))
  (setf (name test-case) (string name))
  (let ((step-forms (cdddr (whole test-case))))
    (setf (steps test-case)
          (generate-steps step-forms))))

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
               (assert (typep string 'string))
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
        for (value foundp) in (multiple-value-list (gethash id hash-table))
        if foundp
          do (protocol-error "Duplicate step ID ~D found." id)
        else do (setf (gethash id hash-table) t)))

(defun check-duplicate-test-phases (test-case)
  (loop with hash-table = (make-hash-table)
        with forms = (cdddr (whole test-case))
        for symbol in (remove-if-not #'symbolp forms)
        for (value foundp) in (multiple-value-list (gethash symbol hash-table))
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

(defmacro define-test-case (&whole whole name (&rest options) &body forms)
  (declare (ignore forms))
  (let ((instance (apply #'make-instance 'test-case
                         :name name :whole whole options)))
    (validate-test-case instance)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *test-cases*) ,instance)
       ',name)))
