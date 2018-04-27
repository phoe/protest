;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use
   #:common-lisp
   #:alexandria
   #:protest/base
   #:protest/common)
  (:shadow #:of-type)
  (:export
   ;; SPECIAL-VARIABLES
   #:*test-cases*
   ;; CLASSES
   #:test-step
   #:test-case
   ;; ACCESSORS
   #:id
   #:test-phase
   #:name
   #:whole
   #:documentation
   #:tags
   #:attachments
   #:steps
   #:steps-list
   #:description
   ;; MACROS
   #:define-test-case))
