;;;; protocol/package.lisp

(defpackage #:protest/protocol
  (:use #:common-lisp
        #:alexandria
        #:named-readtables
        #:protest/base)
  (:shadow #:type)
  (:export #:protocol
           #:protocol-element #:protocol-operation #:protocol-data-type
           #:protocol-function #:protocol-macro #:protocol-class
           #:protocol-condition-type #:protocol-variable #:protocol-category
           #:protocol-config
           #:name #:form #:description #:tags #:dependencies #:exports
           #:elements #:lambda-list #:return-type #:superclasses #:slots
           #:options #:protocol-type #:default-value #:type #:mandatoryp
           #:define-protocol))
