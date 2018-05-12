;;;; src/parachute/package.lisp

(defpackage #:protest/for-parachute
  (:use #:cl
        #:named-readtables
        #:protest/base
        #:protest/test-case)
  (:import-from #:parachute
                #:geq #:*context* #:eval-in-context #:capture-error)
  (:import-from #:protest/test-case
                #:define-test-case)
  (:export
   #:define-test
   #:test-case-result #:test-case-comparison-result
   #:test-case-multiple-value-comparison-result
   #:test-case-finishing-result
   #:true #:false #:is #:isnt #:is-values #:isnt-values
   #:fail #:of-type #:finish
   #:protest/parachute))

(uiop:define-package #:protest/parachute
  (:use)
  (:mix #:protest/for-parachute #:parachute)
  (:reexport #:protest/for-parachute #:parachute))
