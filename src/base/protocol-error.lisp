;;;; src/base/protocol-error.lisp

(in-package #:protest/base)

(define-protocol-condition-type protocol-error (error) ()
  (:documentation
   "The parent condition type for all protocol errors.
\
This condition type is a protocol condition type and must not be instantiated
directly."))

(define-condition protocol-object-instantiation (protocol-error)
  ((symbol :initarg :symbol :reader protocol-object-instantiation-symbol)
   (type :initarg :type :reader protocol-object-instantiation-type))
  (:report
   (lambda (condition stream)
     (format stream "~S is a protocol ~A and thus cannot be instantiated."
             (protocol-object-instantiation-symbol condition)
             (protocol-object-instantiation-type condition)))))

(define-condition simple-protocol-error (protocol-error simple-condition) ())

(defun protocol-error (format-control &rest args)
  (error (make-instance 'simple-protocol-error
                        :format-control format-control
                        :format-arguments args)))
