;;;; src/1am/formatter.lisp

(in-package #:protest/1am)

(defun failure-before (test-case condition)
  (let ((string "Test failure in test ~S, before step 1:~%~A")
        (test-name (name test-case)))
    (format nil string test-name condition)))

(defun failure-during (test-case test-step condition)
  (let ((string "Test failure in test ~A, phase ~A, step ~D:~%~A~%~A")
        (test-name (name test-case)) (phase (test-phase test-step))
        (id (id test-step)) (description (description test-step)))
    (format nil string test-name phase id description condition)))

(defun failure-after (test-case test-step condition)
  (let ((string "Test failure in test ~A, phase ~A, after step ~D:~%~A~%~A")
        (test-name (name test-case)) (phase (test-phase test-step))
        (id (id test-step)) (description (description test-step)))
    (format nil string test-name phase id description condition)))

(defun failure-internal (step-id e)
  (format nil "*CURRENT-TEST-STEP* has an illegal value: ~S
This is an internal PROTEST/TEST-CASE error.~%~A" step-id e))
