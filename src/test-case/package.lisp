;;;; src/test-case/package.lisp

(defpackage #:protest/test-case
  (:use
   #:common-lisp
   #:alexandria
   #:protest/base
   #:protest/common)
  (:shadow #:of-type)
  (:export
   ;; CLASSES
   #:test-step
   #:test-case
   ;; DOCUMENTATION TYPES
   #:test-case
   ;; ACCESSORS
   #:name
   #:package-of
   #:whole
   #:tags
   #:attachments
   #:steps
   #:steps-list
   #:id
   #:description
   #:test-phase
   ;; FUNCTIONS
   #:find-test-case
   ;; MACROS
   #:define-test-case))
