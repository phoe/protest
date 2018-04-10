;;;; test-case/test-case.lisp

(in-package #:protest/test-case)

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
   (%elements :accessor elements
              :initarg :elements
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
  (let ((element-forms (cdddr (whole test-case))))
    (setf (elements test-case)
          (generate-elements element-forms))))

(defun generate-elements (forms)
  (declare (ignore forms)))



(defmacro define-test-case (&whole whole name (&rest options) &body forms)
  (declare (ignore forms)))
