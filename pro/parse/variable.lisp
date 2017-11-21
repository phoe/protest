;;;; parse-variable.lisp

(in-package #:protest)

(defun parse-variable (form docstring)
  `(progn
     ,@(when docstring
         `((setf (documentation ',(first form) 'variable)
                 ,(format nil docstring))))
     ,@(when (>= (length form) 2)
         `((declaim (type ,(second form) ,(first form)))))
     (defvar ,(first form)
       ,@(when (>= (length form) 3) `(,(third form))))))

(pushnew '(:variable #'parse-variable) *categories* :test #'equal)
