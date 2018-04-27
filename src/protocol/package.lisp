;;;; protocol/package.lisp

;; TODO check these
(defpackage #:protest/protocol
  (:use
   #:common-lisp
   #:alexandria
   #:protest/base
   #:protest/common)
  (:export
   ;; SPECIAL VARIABLES
   #:*protocols*
   #:*declaim-types*
   #:*configuration-callback*
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
   #:form
   #:documentation
   #:tags
   #:dependencies
   #:exports
   #:elements
   #:lambda-list
   #:return-type
   #:superclasses
   #:slots
   #:options
   #:protocol-type
   #:default-value
   #:mandatoryp
   #:keyword-types
   #:value-type
   ;; MACROS
   #:define-protocol
   #:execute-protocol))
