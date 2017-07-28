;;;; test/failures.lisp

(in-package #:cl-protest)

(defun failure-before (test-name error)
  (error *failure-before* test-name error))

(defun failure-after (step step-data test-name error)
  (let ((test-step (find (- step 1/2) step-data :key #'car)))
    (destructuring-bind (step description phase) test-step
      (error *failure-after* test-name phase step description error))))

(defun failure-during (step step-data test-name error)
  (let ((test-step (find step step-data :key #'car)))
    (destructuring-bind (step description phase) test-step
      (error *failure-during* test-name phase step description error))))

(defun failure-internal (step)
  (error *failure-internal* step))
