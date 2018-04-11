;;;; test-case/package.lisp

(defpackage #:protest/test-case
  (:use #:common-lisp
        #:alexandria
        #:named-readtables
        #:protest/base)
  (:import-from #:parachute #:geq)
  (:shadow #:of-type)
  (:export #:define-test-case #:define-test
           #:test-case-result #:test-case-comparison-result
           #:test-case-multiple-value-comparison-result
           #:test-case-finishing-result
           #:true #:false #:is #:isnt #:is-values #:isnt-values
           #:fail #:of-type #:finish))
