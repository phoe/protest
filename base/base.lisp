;;;; util/util.lisp

(defpackage #:protest/base
  (:use #:common-lisp
        #:alexandria
        #:closer-mop)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:export #:define-protocol-class
           #:define-protocol-condition-type))

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
         (error ,(format nil "~S is a protocol ~A and thus cannot be ~
instantiated." name string))))
     ',name))

(defmacro defgeneric? (name lambda-list &body options)
  (if (or (not (fboundp name))
          (not (typep (fdefinition name) 'generic-function)))
      `(defgeneric ,name ,lambda-list ,@options)
      `(progn)))

(defun test-protocol-class-instantiate ()
  (unwind-protect
       (define-protocol-class #1=#.(gensym) () ())
    (setf (find-class '#1#) nil)))

(defun test-protocol-condition-type-instantiate ()
  (unwind-protect
       (define-protocol-condition-type #1=#.(gensym) () ())
    (setf (find-class '#1#) nil)))
