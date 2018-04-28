;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use
   #:common-lisp
   #:alexandria
   #:protest/base
   #:protest/common)
  (:shadow #:of-type)
  (:export
   ;; VARIABLES
   #:*test-cases*
   ;; CLASSES
   #:test-step
   #:test-case
   ;; DOCUMENTATION TYPES
   #:test-case
   ;; ACCESSORS
   #:name
   #:whole
   #:tags
   #:attachments
   #:steps
   #:steps-list
   #:id
   #:description
   #:test-phase
   ;; MACROS
   #:define-test-case))
