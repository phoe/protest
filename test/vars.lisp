;;;; test/vars.lisp

(in-package #:cl-protest)

(defvar *tests* (make-hash-table))

(defvar *current-test* nil)

(defvar *current-step* nil)

(defparameter *failure-before*
  "Test failure in test ~A, before step 1:
~A")

(defparameter *failure-after*
  "Test failure in test ~A, phase ~A, after step ~D: ~S
~A")

(defparameter *failure-during*
  "Test failure in test ~A, phase ~A, step ~D: ~S
~A")

(defparameter *failure-internal*
  "*CURRENT-TEST-STEP* has an illegal value: ~S
This is an internal CL-PROTEST error.")
