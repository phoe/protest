;;;; src/common/package.lisp

(defpackage #:protest/common
  (:use #:common-lisp)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
