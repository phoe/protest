;;;; cl-protest-basis.lisp

(in-package #:cl-protest)

(defun choose-function (keyword)
  (ecase keyword
    (:class #'parse-class)
    (:variable #'parse-variable)
    (:macro #'parse-macro)
    (:function #'parse-function)))

(defun parse-form (original-form docstring)
  (destructuring-bind (keyword . form) original-form
    (let ((function (choose-function keyword))
          (docstring (when (stringp docstring) (format nil docstring))))
      (funcall function form docstring))))
