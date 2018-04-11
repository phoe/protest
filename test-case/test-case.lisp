;;;; test-case/test-case.lisp

(in-package #:protest/test-case)

;; TODO put accessor symbols in some sorta BASE package or something so
;; PROTOCOL:DESCRIPTION does not collide with TEST-CASE:DESCRIPTION
(defclass test-step ()
  ((%id :reader id
        :initarg :id
        :initform 0)
   (%description :reader description
                 :initarg :description
                 :initform nil)
   (%test-phase :reader test-phase
                :initarg :test-phase
                :initform nil)))

(defmethod print-object ((test-step test-step) stream)
  (print-unreadable-object (test-step stream :type t)
    (format stream "~A ~D: ~A"
            (test-phase test-step) (id test-step) (description test-step))))

(defmethod make-load-form ((test-step test-step) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots test-step))

(defmethod initialize-instance :after ((test-step test-step) &key)
  (assert (typep (id test-step) 'unsigned-byte))
  (assert (typep (description test-step) '(or string null)))
  (assert (typep (test-phase test-step) 'symbol)))

(defvar *test-cases* (make-hash-table)
  "A hash-table mapping from test case names to test case objects.")

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
           :initform '()))
  (:documentation
   "Describes a test case understood as a series of test phases and test steps
describing each part of the test."))

(defmethod print-object ((test-case test-case) stream)
  (print-unreadable-object (test-case stream :type t)
    (princ (name test-case) stream)))

(defmethod make-load-form ((test-case test-case) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots test-case))

(defmethod initialize-instance :after ((test-case test-case) &key name)
  (when (or (null name) (not (symbolp name)))
    (protocol-error "NAME must be a non-null symbol, not ~S." name))
  (setf (name test-case) name)
  (let ((step-forms (cdddr (whole test-case))))
    (setf (steps test-case)
          (generate-steps step-forms))))

(defun generate-steps (forms)
  (let ((forms forms) (current-phase nil))
    (flet ((make (id description)
             (make-instance 'test-step :id id :description description
                                       :test-phase current-phase)))
      (uiop:while-collecting (collect)
        (do () ((null forms))
          (let ((elt (pop forms)))
            (typecase elt
              (unsigned-byte
               (let ((string (pop forms)))
                 (assert (typep string 'string))
                 (collect (make elt string))))
              (symbol (setf current-phase elt))
              (t (protocol-error "Wrong thing in a test case definition: ~S"
                                 elt)))))))))

(defun validate-test-case (test-case)
  (loop with hash-table = (make-hash-table)
        for test-step in (step test-case)
        for id = (id test-step)
        for (value foundp) in (multiple-value-list (gethash id hash-table))
        if foundp
          do (protocol-error "Duplicate step ID ~D found." id)
        else do (setf (gethash id hash-table) t))
  (loop with hash-table = (make-hash-table)
        with forms = (cdddr (whole test-case))
        for symbol in (remove-if-not #'symbolp forms)
        for (value foundp) in (multiple-value-list (gethash symbol hash-table))
        if foundp
          do (protocol-error "Duplicate test phase ~A found." symbol)
        else do (setf (gethash symbol hash-table) t))
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
    `(progn
       (setf (gethash ',name *test-cases*) ,instance)
       ',name)))
