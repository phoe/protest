;;;; src/base/package.lisp

(uiop:define-package #:protest/base
  (:mix
   #:closer-mop
   #:common-lisp
   #:alexandria)
  (:export
   ;; FUNCTIONS
   #:protocol-object-p
   #:remove-protocol-object
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
