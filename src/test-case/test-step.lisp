;;;; test-case/test-step.lisp

(in-package #:protest/test-case)

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
