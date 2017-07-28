;;;; parse-form.lisp

(in-package #:cl-protest)

(defun choose-function (keyword)
  (ecase keyword
    (:class #'parse-class)
    (:variable #'parse-variable)
    (:macro #'parse-macro)
    (:function #'parse-function)))

(defun validate-name (function-name)
  (cond ((symbolp function-name)
         function-name)
        ((and (listp function-name)
              (= (length function-name) 2)
              (eq 'setf (first function-name)))
         (second function-name))
        (t (error "The provided name is incorrect."))))

(defun parse-form (original-form docstring exportp)
  (destructuring-bind (keyword . form) original-form
    (let ((function (choose-function keyword))
          (docstring (when (stringp docstring) (format nil docstring))))
      `(progn
         ,@(when exportp
             (let* ((function-name (first form))
                    (symbol (validate-name function-name)))
               `((export '(,symbol)))))
         ,(funcall function form docstring)))))
