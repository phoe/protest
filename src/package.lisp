;;;; package.lisp

(uiop:define-package #:protest
  (:use #:common-lisp
        #:protest/base
        #:protest/protocol
        #:protest/test-case
        ;; #:protest/web
        )
  (:reexport #:protest/base
             #:protest/protocol
             #:protest/test-case
             ;; #:protest/web
             ))
