;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use #:common-lisp
        #:alexandria
        #:protest/base
        #:protest/common
        #:protest/1am)
  (:shadow #:of-type)
  (:export #:*test-cases*
           #:test-step #:test-case #:define-test-case
           #:id #:test-phase #:name #:whole #:documentation #:tags #:attachments
           #:steps #:steps-list #:description))
