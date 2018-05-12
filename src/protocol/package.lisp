;;;; src/protocol/package.lisp

(defpackage #:protest/protocol
  (:use
   #:common-lisp
   #:alexandria
   #:protest/base
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
   #:generate-element
   #:generate-forms
   #:generate-code
   #:protocol-element-boundp
   #:protocol-element-makunbound
   ;; FUNCTIONS
   #:find-protocol
   ;; MACROS
   #:define-protocol
   #:execute-protocol))
