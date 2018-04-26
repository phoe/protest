;;;; base/base.lisp

(uiop:define-package #:protest/base
    (:use #:common-lisp
          #:alexandria
          #:closer-mop
          #:protest/1am)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:export #:define-protocol-class
           #:define-protocol-condition-type
           #:protocol-error
           #:protocol-object-instantiation
           #:simple-protocol-error))

(defpackage #:protest/common
  (:use)
  (:export #:tags #:name #:whole))

(in-package #:protest/base)

(defmacro define-protocol-class (name superclasses slots &rest options)
  "Like DEFCLASS, but the defined class may not be instantiated directly."
  `(define-protocol-object defclass "class"
     ,name ,superclasses ,slots ,options))

(defmacro define-protocol-condition-type (name supertypes slots &rest options)
  "Like DEFINE-CONDITION, but the defined class may not be instantiated
directly."
  `(define-protocol-object define-condition "condition type"
     ,name ,supertypes ,slots ,options))

(defmacro define-protocol-object (symbol string name supers slots options)
  `(progn
     (,symbol ,name ,supers ,slots ,@options)
     ;; TODO test this, especially on SBCL
     (defmethod initialize-instance :before ((object ,name) &key)
       (when (eq (class-of object) (find-class ',name))
         (error (make-condition 'protocol-object-instantiation
                                :symbol ',name :type ,string))))
     ',name))

(define-protocol-condition-type protocol-error (error) ()
  (:documentation
   "The parent condition type for all protocol errors.
\
This condition type is a protocol condition type and must not be instantiated
directly."))

(define-condition protocol-object-instantiation (protocol-error)
  ((symbol :initarg :symbol :reader protocol-error-symbol)
   (type :initarg :type :reader protocol-error-type))
  (:report
   (lambda (condition stream)
     (format stream "~S is a protocol ~A and thus cannot be instantiated."
             (protocol-error-symbol condition)
             (protocol-error-type condition)))))

(define-condition simple-protocol-error (protocol-error simple-condition) ())

(defun protocol-error (format-control &rest args)
  (error (make-instance 'simple-protocol-error
                        :format-control format-control
                        :format-arguments args)))
