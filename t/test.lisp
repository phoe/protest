;;;; t/test.lisp

(defpackage #:protest/test
  (:use
   #:cl
   #:protest/1am)
  (:export
   #:*test-packages*
   #:register-test-package
   #:run-all-tests))

(in-package #:protest/test)

(defvar *test-packages* '())

(defun register-test-package (&optional (package *package*))
  (pushnew (package-name (find-package package)) *test-packages*))

(defun run-all-tests ()
  (run *test-packages*))
