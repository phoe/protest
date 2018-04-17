;;;; protocol/package.lisp

(defpackage #:protest/protocol
  (:use #:common-lisp
        #:alexandria
        #:protest/base
        #:protest/common)
  (:export #:*protocols* #:*declaim-types* #:*configuration-setter*
           #:protocol #:define-protocol
           #:protocol-element #:protocol-operation #:protocol-data-type
           #:protocol-function #:protocol-macro #:protocol-class
           #:protocol-condition-type #:protocol-variable #:protocol-category
           #:protocol-config #:category #:config
           #:name #:form #:documentation #:tags #:dependencies #:exports
           #:elements #:lambda-list #:return-type #:superclasses #:slots
           #:options #:protocol-type #:default-value #:mandatoryp
           #:keyword-types #:value-type))
