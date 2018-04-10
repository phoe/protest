;;;; package.lisp

(uiop:define-package #:protest
  (:use #:common-lisp
        #:protest/base
        #:protest/protocol
        #:protest/test-casse
        ;; #:protest/web
        )
  (:shadowing-import-from #:protest/protocol #:type)
  (:reexport #:protest/base
             #:protest/protocol
             #:protest/test-casse
             ;; #:protest/web
             ))
