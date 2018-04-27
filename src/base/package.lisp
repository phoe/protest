;;;; base/package.lisp

(uiop:define-package #:protest/base
  (:use
   #:common-lisp
   #:alexandria
   #:closer-mop)
  (:shadowing-import-from
   #:closer-mop
   #:standard-generic-function
   #:defmethod
   #:defgeneric)
  (:export
   ;; MACROS
   #:define-protocol-class
   #:define-protocol-condition-type
   ;; CONDITIONS
   #:protocol-error
   #:simple-protocol-error
   #:protocol-object-instantiation
   ;; ACCESSORS
   #:protocol-object-instantiation-symbol
   #:protocol-object-instantiation-type))

(defpackage #:protest/common
  (:documentation
   "This package exports symbols common across all PROTEST packages to avoid
conflicts between individual PROTEST packages.")
  (:use)
  (:export #:tags #:name #:whole #:attachments))
