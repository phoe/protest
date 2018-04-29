;;;; src/1am/package.lisp

(defpackage #:protest/1am
  (:use #:cl
        #:named-readtables
        #:protest/base
        #:protest/test-case)
  (:import-from #:1am #:is #:signals #:run #:*tests*)
  (:export #:define-test #:test #:is #:signals #:run #:*tests*))
