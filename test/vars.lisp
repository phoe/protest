;;;; test/vars.lisp

(in-package #:protest)

(defvar *test-packages* (make-hash-table))

(defvar *current-test* nil)

(defvar *current-step* nil)

(defvar *current-step-data* nil)

(defparameter *test-case-not-found*
  "Test case named ~A was not found.
Use DEFINE-TEST-CASE first.")

(defparameter *test-package-not-found*
  "The test package for package ~A was not set.
Use DEFINE-TEST-PACKAGE first.")

(defparameter *failure-before*
  "Test failure in test ~A, before step 1:~%~A")

(defparameter *failure-after*
  "Test failure in test ~A, phase ~A, after step ~D: ~S~%~A")

(defparameter *failure-during*
  "Test failure in test ~A, phase ~A, step ~D: ~S~%~A")

(defparameter *failure-internal*
  "*CURRENT-TEST-STEP* has an illegal value: ~S
This is an internal CL-PROTEST error.")
