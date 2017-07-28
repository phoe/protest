;;;; parse-macro.lisp

(in-package #:cl-protest)

(defun parse-macro (form docstring)
  `(progn
     ,@(when docstring
         `((setf (documentation ',(first form) 'function)
                 ,(format nil docstring))))))
