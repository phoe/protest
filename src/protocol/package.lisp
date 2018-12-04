;;;; src/protocol/package.lisp

(uiop:define-package #:protest/protocol
  (:mix
   #:closer-mop
   #:common-lisp
   #:alexandria
   #:protest/base
   #:protest/ftype
   #:protest/common)
  (:export
   ;; VARIABLES
   #:*protocols*
   #:*config-callback*
   #:*category-callback*
   ;; CLASSES
   #:protocol
   ;; PROTOCOL CLASSES
   #:protocol-element
   #:protocol-operation
   #:protocol-data-type
   ;; PROTOCOL ELEMENTS
   #:protocol-function
   #:protocol-macro
   #:protocol-class
   #:protocol-condition-type
   #:protocol-variable
   #:protocol-category
   #:protocol-config
   ;; DOCUMENTATION TYPES
   #:protocol
   #:category
   #:config
   ;; ACCESSORS
   #:name
   #:canonical-name
   #:bindings
   #:whole
   #:tags
   #:attachments
   #:dependencies
   #:exports
   #:elements
   #:docstring
   #:lambda-list
   #:return-type
   #:keyword-types
   #:declaim-type-p
   #:superclasses
   #:supertypes
   #:slots
   #:options
   #:value-type
   #:initial-value
   #:mandatoryp
   ;; GENERIC FUNCTIONS
   #:keyword-element-class
   #:generate-element-using-class
   #:generate-forms
   #:generate-code
   #:protocol-element-boundp
   #:protocol-element-makunbound
   ;; FUNCTIONS
   #:generate-element
   #:find-protocol
   #:compute-effective-protocol-elements
   ;; MACROS
   #:define-protocol
   #:execute-protocol))
