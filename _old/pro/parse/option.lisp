;;;; parse-option.lisp

(in-package :protest)

;;; TODO do we need this at all if we have *CATEGORIES* as the means
;;; of extending the category list?

(defun parse-option (form docstring)
  ;; TODO implement this
  (declare (ignore form docstring)))

(defun parse-category (form docstring)
  ;; TODO implement this
  (declare (ignore form docstring)))

(pushnew `(:category ,#'parse-category) *categories* :test #'equal)

(pushnew `(:option ,#'parse-option) *categories* :test #'equal)
