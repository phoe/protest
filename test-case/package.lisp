;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use #:common-lisp
        #:alexandria
        #:protest/base
        #:protest/common)
  (:shadow #:of-type)
  (:export #:*test-cases*
           #:test-step #:test-case #:define-test-case
           #:id #:test-phase #:name #:whole #:description #:tags #:attachments
           #:steps #:steps-list))
