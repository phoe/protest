;;;; parse-config.lisp

(in-package #:protest)

(defun parse-config (form docstring)
  (declare (ignore form docstring))
  `(progn))

(pushnew `(:config ,#'parse-config) *categories* :test #'equal)
