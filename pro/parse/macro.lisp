;;;; parse-macro.lisp

(in-package #:protest)

(defun parse-macro (form docstring)
  `(progn
     ,@(when docstring
         `((setf (documentation ',(first form) 'function)
                 ,(format nil docstring))))))

(pushnew '(:macro #'parse-macro) *categories* :test #'equal)
